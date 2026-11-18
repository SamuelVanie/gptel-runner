;;; gptel-runner-discussion.el --- Messages between workflow chats -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Sequence siblings exchange messages through their existing node chats.
;; A pending question saves the path back to the requesting workflow step.
;; Every turn goes through normal call admission and budget accounting.

;;; Code:

(require 'gptel-runner-core)

(defvar gptel-runner--resuming-chats nil
  "Chats whose workflow continuations are currently being reconstructed.")

(defun gptel-runner-handoff-p (run node)
  "Return non-nil when NODE in RUN completes through a handoff tool.
NODE can be a workflow node or its identifier."
  (unless (gptel-runner-node-p node)
    (setq node (gptel-runner--resolve-node run node)))
  (and (eq (gptel-runner-node-kind node) 'agent)
       (not (eq (or (plist-get (gptel-runner-node-properties node) :completion)
                    (plist-get (gptel-runner-run-options run) :completion))
                'response))))

(defun gptel-runner--adjacent-agent (run node backward)
  "Find NODE's next agent in the linear sequence in RUN, or previous if BACKWARD."
  (let ((parents (cdr (reverse (gptel-runner-node-path run node))))
        sequence stages)
    (while (and parents (eq (gptel-runner-node-kind (car parents)) 'sequence))
      (setq sequence (pop parents)))
    (when sequence
      (cl-labels ((collect (stage)
                    (pcase (gptel-runner-node-kind stage)
                      ('sequence (mapc #'collect (gptel-runner-node-children stage)))
                      ('elisp nil)
                      (_ (push stage stages)))))
        (collect sequence))
      (let ((neighbor (cadr (memq node (if backward stages (nreverse stages))))))
        (and neighbor (gptel-runner-handoff-p run neighbor) neighbor)))))

(defun gptel-runner-previous-agent (run node)
  "Return the preceding completed discussion partner of NODE in RUN.
NODE can be a workflow node or its identifier.  Return nil at a boundary."
  (unless (gptel-runner-node-p node)
    (setq node (gptel-runner--resolve-node run node)))
  (when-let* (((gptel-runner-handoff-p run node))
              (previous (gptel-runner--adjacent-agent run node t))
              ((eq (gptel-runner-node-state run previous) 'succeeded)))
    previous))

(defun gptel-runner--message (run kind call to message)
  "Record delivery of MESSAGE of KIND from CALL in RUN to node ID TO."
  (let ((node (gptel-runner-call-node call)))
    (gptel-runner--emit run kind node call
                        (list :from (gptel-runner-node-id node)
                              :to to :message message))))

(defun gptel-runner--accept-agent-result (call parsed raw)
  "Commit the PARSED node output and accepted RAW message from CALL."
  (let* ((run (gptel-runner-call-run call))
         (node (gptel-runner-call-node call))
         (handoff (and (stringp raw) (gptel-runner-handoff-p run node) raw)))
    (when-let* ((key (plist-get (gptel-runner-node-properties node) :save-as)))
      (gptel-runner-put run key parsed))
    (gptel-runner--set-node-state run node 'succeeded parsed handoff)
    (when handoff
      (let ((next (gptel-runner--adjacent-agent run node nil)))
        (gptel-runner--message run 'node-handoff call
                               (and next (gptel-runner-node-id next)) handoff)))))

(defun gptel-runner--incoming-handoff (run node)
  "Return the accepted handoff addressed to NODE in RUN, if any."
  (when-let* (((gptel-runner-handoff-p run node))
              (previous (gptel-runner-previous-agent run node))
              (id (gptel-runner-node-id previous))
              (message (plist-get (gethash id (gptel-runner-run-node-outcomes run))
                                  :handoff)))
    (format "Handoff from %s:\n%s" id message)))

(defun gptel-runner--route-discussion (run owner stack call value)
  "Route a question or reply VALUE from CALL along STACK for OWNER in RUN.
Return the next turn's checkpoint.  STACK starts with the current speaker
and ends with the workflow step that must eventually produce an output."
  (let* ((asking (gptel-runner-call-follow-up-p call))
         (previous (and asking (gptel-runner-previous-agent
                                run (gptel-runner-call-node call))))
         (next (if previous (cons (gptel-runner-node-id previous) stack)
                 (cdr stack))))
    (unless (and (stringp value) (not (string-blank-p value))
                 (if asking previous next))
      (error "Invalid %s from node %s" (if asking "question" "reply") (car stack)))
    (let* ((prompt (format (if asking
                               "Follow-up question from %s:\n%s\n\nAnswer with RunnerHandoff."
                             "Reply from %s:\n%s\n\nContinue your task.")
                           (car stack) value))
           (turn (list :stack next :prompt prompt
                       :repair (gptel-runner-call-repair-p call))))
      (puthash owner turn (gptel-runner-run-discussions run))
      (gptel-runner--message run (if asking 'node-follow-up 'node-reply)
                             call (car next) value)
      turn)))

(defun gptel-runner--submit-discussion (run node agent prompt done repair-p)
  "Schedule a turn for NODE and AGENT in RUN with PROMPT.
Preserve DONE and REPAIR-P across any follow-up exchanges."
  (let* ((owner (gptel-runner-node-id node))
         (turns (gptel-runner-run-discussions run))
         (turn (gethash owner turns))
         (generation (gptel-runner-run-generation run)))
    (cl-labels
        ((receive (stack call state value)
           (cond
            ((not (eq state 'succeeded)) (funcall done call state value))
            ((/= generation (gptel-runner-run-generation run)) nil)
            ((or (cdr stack) (gptel-runner-call-follow-up-p call))
             (let ((next (condition-case err
                             (gptel-runner--route-discussion run owner stack call value)
                           (error (funcall done call 'failed
                                           (list :type 'invalid-message :error err))
                                  nil))))
               (when (and next (eq (gptel-runner-run-state run) 'running)
                          (= generation (gptel-runner-run-generation run)))
                 (submit (plist-get next :stack) (plist-get next :prompt)))))
            (t (remhash owner turns)
               (funcall done call state value))))
         (submit (stack text)
           (let* ((peer (and (cdr stack) (gptel-runner--resolve-node run (car stack))))
                  (speaker (if peer (gptel-runner--agent
                                     (plist-get (gptel-runner-node-properties peer) :agent))
                             agent))
                  (callback (apply-partially #'receive stack))
                  (parked (gptel-runner--resuming-chat run node)))
             (if parked
                 (setf (gptel-runner-call-on-complete parked) callback)
               (gptel-runner--submit-call run (or peer node) speaker text callback
                                          repair-p (and peer owner))))))
      (submit (or (plist-get turn :stack) (list owner)) prompt))))

(defun gptel-runner--resuming-chat (run node)
  "Find the parked call awaiting a reconstructed continuation for NODE in RUN."
  (cl-find-if
   (lambda (call)
     (and (eq run (gptel-runner-call-run call))
          (eq (gptel-runner-call-state call) 'waiting-feedback)
          (equal (gptel-runner-node-id node)
                 (or (gptel-runner-call-discussion-owner call)
                     (gptel-runner-node-id (gptel-runner-call-node call))))))
   gptel-runner--resuming-chats))

(defun gptel-runner--validate-discussions (run)
  "Check pending conversation routes against RUN's current workflow."
  (maphash
   (lambda (owner turn)
     (let ((stack (plist-get turn :stack)))
       (unless (and (consp stack) (proper-list-p stack)
                    (equal owner (car (last stack)))
                    (stringp (plist-get turn :prompt))
                    (cl-loop for (speaker requester) on stack
                             for node = (gptel-runner-find-node run speaker)
                             always
                             (and node (gptel-runner-handoff-p run node)
                                  (or (not requester)
                                      (when-let* ((parent (gptel-runner-find-node run requester)))
                                        (eq node (gptel-runner-previous-agent run parent)))))))
         (user-error "Saved conversation for %S has an invalid return path" owner))))
   (gptel-runner-run-discussions run)))

(provide 'gptel-runner-discussion)
;;; gptel-runner-discussion.el ends here
