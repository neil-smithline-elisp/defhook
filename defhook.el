(defgroup defhook nil "Define mode hooks wherever you want." 
  :link '(function-link defhook))

(defun defhook-validate-separator (separator)
  "Make sure that SEPARATOR is a valid \"punctuation\" character.
Ensure that SEPARATOR is either a character or a string of length
one such that:

  (with-syntax-table emacs-lisp-mode-syntax-table
   (= ?_ (char-syntax separator)))

evaluates to non-null. 

The values of `defhook-user-separator' and
`defhook-sym-desk-separator' must return true for this
function. 

See `defhook-create-function-name' for the more details of the
the function name`s `defhook' generates."
  (assert (char-or-string-p separator) t)
  (when (characterp separator) (setq separator (string separator)))
  (assert (= 1 (length separator)) t)
  (with-syntax-table emacs-lisp-mode-syntax-table
    (assert (= ?_ (char-syntax (string-to-char separator))) nil
	    "SEPARATOR %s is not a valid \"punctuation\" character."
	    separator))
  ;; It's all good, let's return.
  separator)

(defcustom defhook-user-prefix nil
  "Prefix to use on hooks created with `defhook'.
The default value is the user name as returned by
`user-login-name'. 

A well chosen prefix will prevent the names of the generated hook
functions from conflicting with existing functions. Your login
name is used as a default because it is easy to obtain and likely
unique. But any three or four character string should be
sufficient.

I use my initials, \"ngs\". I originally used \"ns\" but Aquamacs
uses \"ns\" as a prefix for many functions so I included my
middle initial. If your name happens to be \"George Edward
Thomas\", initials \"get\", you probablyshould consider using
something besides your initials.

If you don't care about the length the generated function names
you can leave the default value if your username is relatively
uncommon or set the prefix to your email address or your
website's URL.

See `defhook-validate-separator' for more details about
the legal values for this."
  :type                 'string
  ;; `:set' defaults to set-default but this var shouldn't be bufer local
  ;; so we define our own value of it.
  :set                  (lambda (sym value) 
                          (assert (not (null value)))
                          (set sym value))
  ;; Respect a user set value and prohibit a null value
  :initialize           (lambda (sym value)
                          (unless (and (boundp sym) (symbol-value sym))
                            (set sym (user-login-name))))
  :risky                t
)

(defcustom defhook-user-separator ":"
  "Punctuation used in `defhook's generated function name.
The value of the separator must be a string of exactly one
character. The character must be a \"punctuation\" character such
as a hyphen or a period.

While words in Emacs lisp symbol names are traditionally
separated by hyphens, using a different punctuation character,
such as colon (\":\") can help to keep your hook names unique and
easy to find.

See `defhook-create-function-name' for more details about how
`defhook-user-separator' is used in the name of `defhook's
function name.

See `defhook-validate-separator' for more details about legal
values."
  :type                 'string
  ;; `:set' defaults to set-default but this var shouldn't be buffer
  ;; local so we create our implement :set ourselves. 
  ;;
  ;; As a note to any `customize' package authors, `:set' shouldn't
  ;; evaluate to `set-default' when `:risky' is non-nil.
  :set                  (lambda (sym value) 
                         (set sym (defhook-validate-separator value)))
  :risky                t
)

(defcustom defhook-sym-desc-separator ":"
  "Punctuation used in `defhook's generated function name.
The `defhook-user-prefix' is begins the name of `defhook's
generated function. The `sym-desc', a brief description of the
hook's functionality, is the suffix. The
`defhook-sym-desc-separator' is the punctuation used to separate
the `sym-desc' from the rest of the function name."
  :type                 'string
  ;; `:set' defaults to set-default but this var shouldn't be bufer local
  ;; so we define our own value of it.
  :set                  (lambda (sym value) 
                         (set sym (defhook-validate-separator value)))
  :risky                t
  )

(defcustom defhook-display-informational-messages 'after-startup
  "Adjust the verbosity of `defhook's informational messages.
`defhook' provides informational messages when it is called. 

Setting `defhook-display-informational-messages' to nil will
suppress all of `defhook's informational messages. Setting it to
t will enable all informational messages.

The default value, `after-startup', will cause `defhook' to
suppress informational messages during emacs startup
`emacs-startup-hook's This tends to reduce a lot of noise during
boot time.

Note that error messages are always generated. This variable only
affects informational messages."
:type			'boolean ;; FIXME wrong type
)

(defcustom defhook-use-font-lock-mode t
  "Enable special `font-lock-mode' highlighting for `defhook' forms.
Unless you are unhappy with how `font-lock-mode' interacts with
`defhook' you will probably not wish to change this setting.

By default, `defhook' adss a hook function to `emacs-lisp-mode'
instructing `font-lock-mode' to fontify `defhook' declarations.
Setting this to a nil value will disable `font-lock-mode'
decorations the next time you run Emacs."
  :type			'boolean
)

(defvar defhook-last-informational-message  nil
  "The most recent informational message from `defhook'.
See `defhook-display-informational-messages' and `defhook-done'
for more details.")

(defun defhook-partial-name (sym separator)
  "Create part of the generated function name for `defhook'.
String SYM or the `symbol-name' of SYM if it is a symbol,
concatenated with the punctuation character SEPARATOR, will
become part of a unique hook function name that is defined via
`defhook'. 

Unless SEPARATOR equals \"\", it will be passed to
`defhook-validate-separator' for a more accurate description of
valid values for separator."

  ;; We should always be called from `defhook' which ensures that SYM
  ;; is never null.

  (let* ((our-sym-name (if (symbolp sym) (symbol-name sym) sym))
	 (our-separator (if (string= "" separator) 
			    separator
			  (defhook-validate-separator separator))))
    (assert (stringp our-sym-name) t "Argument SYM=`%s' must be a string or a symbol.")

    ;; By here, our-separator is a string of length 0 ("") or 1.  We
    ;; need to make sure the string is a valid word delimiter. 
    ;; 
    ;; IMPLEMENTATION NOTE: This test relies on the fact that
    ;; `string-to-char' converts a zero-length string to the character
    ;; with value 0 and that char-syntax says that character is of the
    ;; syntactical type ?_.
    (assert (with-syntax-table emacs-lisp-mode-syntax-table
              (= ?_ (char-syntax (string-to-char our-separator))) t 
              "SEPARATOR `%s' must be a \"word delimiter\"."
              our-separator))
    ;; So we're good. our-separator is, well it's our separator
    (concat our-sym-name our-separator)))


(defun defhook-check-sym-syntax (sym arg-name)
 "Generate an error if SYM is nil or not a symbol. 
This is intended to be used to validate parameters passed to
`defhook'. SYM does not need to be assigned a value or even
`intern'ed as it is going to be used as part of a function's name
from `defhook'.

The second argument, ARG-NAME, is a string that will be used in
an error message if one is generated.

Return SYM when there's no error."
 (unless (and sym (symbolp sym))
    (error "%s (value=%s) must be a valid symbol." arg-name sym))
 sym)

(defun defhook-delayed-done (func-sym hook-sym 
                                      hook-pending &rest hook-args)
  '(message "`defhook' added `%s' to `%s' because `%s' was loaded."
           func-sym hook-sym hook-pending))

(defun defhook-executed (func-sym hook-sym 
				   &rest hook-args)
  '(message "`defhook' executed `%s' on hook `%s'."
           func-sym hook-sym))

(defun defhook-done (func-sym hook-sym op began-in-hook
                              hook-pending &rest hook-args)
  "Display an informational `message' describing `defhook's actions.
The value of `defhook-display-informational-messages' controls
these messages.

Whether or not an informational message is printed, the variable 
`defhook-last-informational-message' will be set to the the text of
the most recent message.

`defhook-done' returns the symbol of the generated function's name."
  (let ((msg-fmt)
	(whole-format))
    
    ;; This cond assigns msg-fmt to the variable portion of the
    ;; informational message. Writing it made me ill. I recommend you
    ;; don't read it.
    (cond ((eq 'no-op op)
           (if (member func-sym hook-sym) 
               (setq msg-fmt 
		     "called in disabled mode and left `%s' in")
             (setq msg-fmt 
		   "called in disabled mode and did not find `%s' in")))
          ((eq 'delete op)
           (if began-in-hook
               (setq msg-fmt 
		     "removed `%s' from")
             (setq
	      msg-fmt 
	      "did not need to remove `%s' because it wasn't in")))
          (hook-pending
           (setq 
            msg-fmt 
            (concat 
             (if began-in-hook
                 "removed `%s' and" 
               "will create and the function `%s'")
             " when `"
             (symbol-name hook-pending)
             "' is loaded and add it to the front of")))
           (t 
            (if began-in-hook
                (setq msg-fmt 
                      "redefined `%s' and moved to the front of")
              (setq msg-fmt 
                    "created `%s' and added to the front of"))))

    ;; Now we add to msg-fmt to create the entire message.
    (setq whole-format (concat "`defhook' " msg-fmt " hook `%s'. "))
    (setq defhook-last-informational-message
	  (concat (format whole-format func-sym hook-sym) 
		  (format "The current value of `%s' is `%s'." 
                          hook-sym (symbol-value hook-sym))))
   (when
        (or (eq t defhook-display-informational-messages)
            (and after-init-time    ; Set `after emacs-startup-hook' is done
                 (eq 'after-startup defhook-display-informational-messages)))
      (message defhook-last-informational-message)))
  func-sym)

(defun defhook-create-function-name (name hook-name)
  "Create the name of the function to be generated in a call to `defhook'.
NAME and HOOK-NAME are the arguments that were passed to
`defhook'.

`defhook' attempts to name each function with a descriptive and
unique name. This is useful when examining the value of a hook
function or when calling one interactively.

As an example of descriptive function names, at the time of
writing this, if I start Emacs without having run my
`user-init-file', the value of `text-mode-hook' is:
    (smart-spacing-mode auto-detect-wrap)

When a hook function is generated by `defhook', the name will be
comprised of:
    - `defhook-user-prefix'
    - `defhook-user-separator'
    - the name of the hook (ie: the HOOK-SYM argument passed to `defhook')
    - `defhook-sym-desc-separator'
    - the NAME argument passed to `defhook'

Assuming you have not changed the default values of any of the
above settings, your username is \"neil\" and this is your
`defhook' declaration:
     (defhook ignore-case (dired-mode-hook) 
       \"Always ignore case in `dired-mode' searches.\"
       (setq case-fold-search t))

Then the generated function name will be:
    \"neil:dired-mode-hook:ignore-case\"

All of my generated hook function can be found by calling
`apropos-command' and searching for \"^neil:\".

Similarly, if I run `apropos-command' and search for
\"dired-mode\", the functions are listed alphabetically so that
all of my generated hook functions will easy to locate. This is
the list of functions I get when running the above
`apropos-command':
    dired-mode
    ngs:dired-mode-hook:ignore-case
    ngs:dired-mode-hook:my-key-bindings
    turn-on-gnus-dired-mode
    wdired-change-to-wdired-mode

As you can see, I have two generated hook functions for
`dired-mode'. One that disables case-sensitivity in searches and
the other that customizes some key bindings."
  (concat
   (defhook-partial-name defhook-user-prefix defhook-user-separator)
   (defhook-partial-name hook-name defhook-sym-desc-separator)
   (defhook-partial-name name "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro* defhook (name (hook-sym &key 
				   (op 'add)
                                   (interactive-spec t)
				   (append nil)
				   (validate-hook-name t)
                                   (eval-after nil)
                                   (local nil)
                                   (hook-args nil))
                         &rest body)
  "Create a hook function and add it to the appropriate hook.

`defhook' simplifies the use of hooks (see the Info node `Hooks'
for background information) by providing an easy means of
creating, documentating, tracking, and sharing hooks.

================================================================
                           SYNTAX
================================================================

For those of you who are masochists, you can interpret the above
'gobbledygook' (yes, 'gobbledygook' is a technical term), using
the Info node `(CL)Argument Lists' as reference.

For the rest of you, just read on.

A basic `defhook' declaration is similar to a `defun'
declaration:

    (defhook NAME ARGLIST &optional DOCSTRING &rest BODY) 

DOCSTRING and BODY have the same meaning as the similarly named
parameters of `defun'. An example of `defhook' in my
`user-init-file' is:
     (defhook ignore-case (dired-mode-hook) 
       \"Always ignore case in `dired-mode' searches.\"
       (setq case-fold-search t))

This definition will add a function to `dired-mode-hook' so that
any buffer that enters `dired-mode' will ignore case when
searching.

NAME, in this example \"ignore-case\", should be a brief
description of the hook does. For example, \"show-line-numbers\"
and \"tab-width-8\". NAMEs such as \"my-hook\" are likely a poor
choice.

If you are interested, the algorithm for naming the generated
function is described in `defhook-create-function-name'.

================================================================
                     MORE COMPLEX USE CASES
================================================================

For most uses of `defhook', the above description is all that is
nededed to use `defhook'. That said, the following additional
options are available.

`defhook' uses Common Lisp keywords as described in the Info
node `(CL)Argument Lists'. Each keyword is an optional argument
you can pass to `defhook'. If you do not specify a keyword, its
default value will be used.

While the function definition shows each keyword unadorned, when
they are used they must be prefixed by a colon (':'). For
example, the INTERACTIVE-SPEC keyword should always be referred
to as :INTERACTIVE-SPEC. Note that you should use the lower-case
form of the keywords in your code. They are only capitalized
because of Emacs documentation standards.

Each keyword is shown as a two-item list in the above function
definition (AKA: 'gobbledygook'). They are shown between the
`&key' and `&rest' separators. The `car' of each list is the
keyword name and the `cadr' is the default value.

For example, the :INTERACTIVE-SPEC keyword has a default value of
t. Similarly, the :OP keyword has a default value of \"(quote
add)\" or, more simply, \"'add\".

The meaning of each keyword is as follows:

:INTERACTIVE-SPEC - Default value t. If non-nil, the created
function will be callable as an interactive function. This is
typically helpful when you are editing a hook as it allows you to
test it execute-extended-command.

:OP - Default value 'add. The default value of 'add will add the
generated function to the front of the hook. If the function is
already in the hook, it will be moved to the beginning of the
hook. A value of 'delete will remove the generated function from
the hook if it already exists. Finally, a value of 'no-op will
have no effect on the hook.

:APPEND - Default value nil. A non-nil value will cause `defhook'
when :OP is 'add will cause `defhook' to append rather than
prepend the generated function to the hook. This keyword
corresponds to the APPEND argument of the `add-hook' function.

:EVAL-AFTER - Default value nil. If you wish your `defhook'
declaration to only be executed after a specific feature has been
defined (see `featurep'), you can accomplish this by setting the
value of :EVAL-AFTER keyword to the feature. The feature should
be an unquoted symbol as in \":eval-after dired\".

:LOCAL - Default value nil. By default, all generated functions
are added to their global hook value. Setting :LOCAL to a non-nil
value will only modify the buffer-local hook value. This keyword
corresponds to the LOCAL argument of the `add-hook' function.

:HOOK-ARGS - Default value nil. While most hooks do not take any
arguments, hooks such as `window-scroll-function' do take
arguments. If you are generating a function for a hook that takes
arguments, you should pass the arguments as a list in
the :HOOK-ARGS keywords. These arguments will be available for
use within the generated function. Using `window-scroll-function'
as an example, the :HOOK-ARGS keyword should be
\":HOOK-ARGS (window new-display-start)\".

:VALIDATE-HOOK-NAME - Default value t. When non-nil, some
rudimentary checks are performed to see if the HOOK-SYM appears
to be a valid hook and generate an error if it is invalid. Even
when validating is enabled, many invalid HOOK-SYMs will be
accepted. The :VALIDATE-HOOK-NAME keyword should typically be
left as its default value. The only reason for changing it to nil
is if you are dealing with a hook that has an atypical name such
as `hook-for-foo' instead of the standard `foo-hook'."
  
  (declare (doc-string 3))
  (assert (not (null defhook-user-prefix)) t)
  (let* ((post-hook-action      t)      ; Unimplemented keyword
         (post-defhook-action   t)      ; Unimplemented keyword
	 (our-name		(symbolp name))
         (our-local             local)
         (our-append            append)
	 (our-op		(assert-rtn 
				 op (member op '(add delete no-op)) t))
	 (our-hook-sym		(defhook-check-sym-syntax 
				  hook-sym "HOOK-NAME"))
	 (our-hook-args		hook-args)	
	 (tmp-int-spec		interactive-spec) ; avoid a double-eval
	 (our-int-spec		 (if (eq tmp-int-spec t) 
				     (interactive)
				   tmp-int-spec))
         (our-val-hook-name     validate-hook-name)
	 ;; nil is the default value for `eval-after' but if we leave
	 ;; it as null we need to special case that later on, in the
	 ;; most complex part of the code. Being that the feature
	 ;; 'simple is always loaded we'll set `eval-after' to 'simple
	 ;; if it is null. Then there is no need to special case the
	 ;; later code
         (our-eval-after        (or eval-after 'simple))
	 (our-hook-name		(assert-rtn 
				 our-hook-sym
				 (or (not our-val-hook-name)
                                     (string-match-p
                                      "-\\(hook\\|function\\|hooks\\|functions\\)$"
                                      (symbol-name our-hook-sym)))
                                 t))
	 (our-func-name		(defhook-create-function-name 
				  (defhook-check-sym-syntax name "NAME")
				  our-hook-sym))
	 (our-func-sym		(intern our-func-name))
         (began-in-hook         (member our-func-sym
					(if (boundp our-hook-sym) 
					    (symbol-value our-hook-sym)
					  (set our-hook-sym nil))))
	 (our-body		body) 
	 (our-docstring		(when (stringp (car our-body))
				  (setq our-docstring (pop our-body))))
	 (defhook-done-form    `(defhook-done ',our-func-sym
                                  ',our-hook-sym ',our-op
                                  ',began-in-hook 
                                  'nil
                                  ',our-hook-args)))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; Now we have all our variables. They have all passed validity
    ;; checking and, assuming I didn't mess up, using the our-*
    ;; variables will ensure that the macro arguments have been
    ;; evaluated the correct number of times.
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (unless :Comment-Ignore-Test-Code
            (defhook test1 (foo-hook) 
              (message "foo-hook-test1")
              (message "foo-hook-test1 again"))
            (defhook test2 (foo-hook) (message "foo-hook-test 2"))
            )
    
    (cond ((eq 'no-op our-op)           ; Doing nothing is easy
	   `,defhook-done-form)         ; Just run the status function
	  
          ;; We need to remove the named hook. 
	  ;; '
	  ;; Our local variable, began-in-hook is used by defhook-done
          ;; to tell the user whether we removed anything or not.
	  ((eq 'delete our-op)
	   (remove-hook our-hook-sym our-func-sym)
	   `,defhook-done-form)

          ;; At last. The fun stuff.
          (t 
	   ;; For now, we always move the hook to the front so we
	   ;; remove it first. Then it gets added to the beginning
	   ;; when we add it later
	   (remove-hook our-hook-sym our-func-sym)


           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; Depending on the exact usage, there are two or three
           ;; bookkeeping functions called during the creation and
           ;; execution of the hook.
           ;;
           ;; 1) `defhook-done' is always c called just before
           ;; `defhook' returns. This is happens for all `defhook'
           ;; operations but is mentioned here beacuse it interacts
           ;; with other bookkeeping functions.
           ;;
           ;; 2) If the creation of the hook is delayed due to
           ;; `eval-after' requirements, `defhook-delayed-done' will
           ;; be called when the hook is actually created. If the hook
           ;; is never created, `defhook-delayed-done' is never
           ;; called. Care should be taken as sometimes
           ;; `defhook-delayed-done' will be called in the middle of a
           ;; bunch of library `load's and `require's where parts of a
           ;; package have been evaluated while other parts have not.
           ;;
           ;; 3) Each time the hook is actually run, even if there is
           ;; an error in `hook-body' that stops the completion of the
           ;; hook function, `defhook-ran' will be called.
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; Important Note: The `defhook-delayed-done' function and
           ;; the `defhook-executed' function are not evaluated in the
           ;; context of `defhook'. There can be no unbound references
           ;; unless they are to globally available symbols. For
           ;; example, a global function that won't be changed.
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           (let (
                 ;; This defines the `defhook-delayed-done' function.
                 ;; We create the `defhook-delayed-done' on every call,
                 ;; even if it won't be called. It keeps the code
                 ;; simpler.  See "Important Note" above about binding.
                 (defhook-delayed-done-form 
                   (list 'defhook-delayed-done
                         (list 'quote our-func-sym)
                         (list 'quote our-hook-sym)
                         (list 'quote 'our-eval-after)))

                 ;; This defines the `defhook-execute' function. See
                 ;; "Important Note" above about binding.
                 (defhook-executed-form 
                   (list 'defhook-executed
                         (list 'quote our-func-sym)
                         (list 'quote our-hook-sym))))
            
             ;; This is the the part of the macro that will be expanded.
             `(let ((pending-load ',our-eval-after))
                (eval-after-load ',our-eval-after
                  '(progn 
                     (defun ,our-func-sym ,our-hook-args
                       ,our-docstring
                       ,our-int-spec
                       (unwind-protect
                           (progn ,@our-body)
                       ,defhook-executed-form))
                     (add-hook ',our-hook-name #',our-func-sym ,our-append ,our-local)
                     (if (and (boundp 'pending-load) pending-load)
                         (setq pending-load nil)
                       ,defhook-delayed-done-form)))
                (defhook-done ',our-func-sym ',our-hook-sym ',our-op ',began-in-hook
                  pending-load ',our-hook-args)))))))

(defvar defhook-emacs-startup-hook-monitor nil 
  "True if `defhook-emacs-startup-hook-monitor' has run, nil otherwise.")

(defhook defhook-emacs-startup-hook-monitor (emacs-startup-hook)
  "Used by `defhook-startup' to determine if `emacs-startup-hook' has run."
  (setq defhook-emacs-startup-hook-monitor t))

(defun defhook-startup ()
  "A bit of a hack to call `emacs-startup-hook' when your init file is broken.
While no promises can be made in the face of a broken init file,
this function tries to make sure that `emacs-startup-hook' has
been called exactly once.

Calling `defhook-startup' if `emacs-startup-hook' has run successfully will do nothing. "
  (interactive) 
  (unless defhook-emacs-startup-hook-monitor
    (run-hooks 'emacs-startup-hook)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add defhook to font-lock keywords for emacs mode.
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; font-lock-mode is optimized to make adding to the list of keywords
;; very efficient. But, alas, it allows duplicates. One has identical
;; copies of the same mapping,

;; Removing the items from the keyword list is even worse. If you
;; remove the same item twice without adding the item in between, then
;; adding it again will not work.

;; Becasue we use a constant for the regexp, `delq' will efficiently
;; remove all instances of the mapping from the keyword alist and make
;; things work better.

(defconst defhook-font-lock-keywords 
  '(("(\\(defhook\\)[[:space:]]+\\([-[:word:]]+\\)[[:space:]]+(\\([-[:word:]]+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face)
      (3 font-lock-variable-name-face)))
  "`font-lock-mode' regexp for `defhook' in `emacs-lisp-mode'.")

(when defhook-use-font-lock-mode
  (let ((defhook-user-prefix "defhook"))
    (defhook add-defhook-keywords (emacs-lisp-mode-hook)
      "Add `defhook' keywords to `emacs-lisp-mode's `font-lock-keywords'."
      (font-lock-add-keywords nil defhook-font-lock-keywords))))

(provide 'defhook)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; THE FOLLOWING CODE IS COMMENTED OUT
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless :Comment-not-yet-implemented


(defmacro defhook-startup-hook-monitor (func-sym)
    "Internally used by `defhook' to track execution of startup hooks.
Unless you're name is `defhook' don't use this.

By categorizing hooks based on when they are run, the need for
special handling of `emacs-startup-hook' becomes more
apparent. Note that Emacs categorizes based on whether the hook
functions are passed arguments or not (see Info
node `(elisp)Hooks'). These two categorizations are valid,
independent, and orthogonal of each other. 

The most common hooks are \"mode hooks\". For this
categorization, a mode hook is any hook that is triggered by the
entering or exiting of a specific mode. `shell-mode-hook' and
`view-mode-hook' are common examples. 

I'll use the term \"event hooks\" to describe any hook that is
neither a mode hook nor a \"lifecycle hook\" (defined
below). These hooks are run when a specific event occurs. For
example, `write-file-hooks' occur whenever you are writing a
file, `after-undo-hook' occurs after every undo operation, and
`exit-language-environment-hook' occurs whenever you change the
current language (written language, not programming language).

\"Lifecycle hooks\" are run at specific points in the lifecycle
of Emacs and only at those times. for example,
`emacs-startup-hook', probably the most common lifetime hook, is
run after Emacs has been started and has completed reading the
initialization files. `after-init-hook' is another lifecycle
hook. While there may be exceptions, the key concept behind
lifecycle hooks is that, under normal circumstances, they are run
exactly once.

Mode hooks are run every time the associated modes are
entered. For example, every time you load a \".java\" file, the
`java-mode-hook' functions are executed. You can even force
re-execution of the hooks by executing
\"\\[execute-extended-command] java-mode\" while in a buffer that
is already in `java-mode'.

Other lifecycle hooks, such as `before-make-frame-hook', may not
work if run a second time on the same frame, but they are still
run every time a frame is created.

Writing and debugging a lifecycle hook such as
`emacs-startup-hook' generally involves exiting and restarting
Emacs ot test the hook's functionality. While less than ideal,
this is not a major problem. But `defhook' encourages
implementing multipe hook functions, frequently throughout one or
more elisp files, for the same hook. While this provides you with
a means of colocating the the functionality implemented in a hook
with the code that requires that functionality, it does increase
the burden of testing lifecycle hooks.

As such, `defhook' tries to help you out by keeping track of
lifecycle hooks (currently only `emacs-startup-hook') and using
that information to allow testing of Emacs lifecycle hooks
without recreating the actual lifecycle event.

I am continually frustrated when I start emacs and my startup
fails due to a stray character inside of my initialization
file (typically an \"xo\"). I want to fix the typo and the just
execute \\[eval-buffer] but this doesn't run the lifecycle hooks
that would have been run in a normal startup."
    ;; Imagine clever code here.

    (unless (assoc func-sym defhook-startup-hook-monitor))
    

    )


(defvar defhook-hook-status nil 
  "Status of hooks `defhook' has created and when they've been run.
An association list used to maintain the current status of
hooks created by `defhook'. For example:
    ((emacs-startup-hook . <hook-function information>) 
     (lisp-interaction-mode . <hook-function information>)
     (find-file-hook . <hook-function information>)
     ......)

Only hooks that have been passed to defhook will have entries. No
hook will have more than one entry. The ordering of the hooks in
the association list is undefined.

Each <hook-function information> is an association list whose key
is a function that `defhook' has generated. Every generated
function will be the key of exactly one hook-function information
in the association list. For example, the entry in
`defhook-hook-status' for `emacs-startup-hook' could be:
    (emacs-startup-hook . 
        ((ngs:emacs-startup-hook:bind-fill-paragraph . <details>)
         (ngs:emacs-startup-hook:configure-buffer-mngmt . <details>)
         (ngs:emacs-startup-hook:dired-settings . <details>)))

The <details> for each hook function are documented in the
code (sorry but they are changing faster than I want to bother to
update this documentation :-D).")

(defun defhook-update-hook-status (func-sym hook-sym status)
  "Update defhook-hook-status for FUNC-SYM of HOOK-SYM with STATUS.
`defhook' will directly or indirectly call
`defhook-update-hook-status' with one of the following values:

'created:  FUNC-SYM was created and added to HOOK-SYM
'deleted:  FUNC-SYM was removed from HOOK-SYM
'updated:  FUNC-SYM which was in HOOK-SYM, was updated
'executed: FUNC-SYM in HOOK-SYM was executed, presumably by `run-hooks'

Any other value is illegal."
  (assert (memq status '(created deleted updated executed)) t)
  (let* ((hook-status   (cdr (or (assq hook-sym defhook-hook-status)
                                 (push (cons hook-sym nil)))))
         (func-status   (cdr (or (assq func-sym hook-status)
                                 (push (cons func-sym '(nil nil)))))))
    (case 'status
      'created      ()
    )))

(unless :Comment-Test-Code
  (progn 
    (setq foo-mode-hook nil)
    (defhook hello (foo-mode-hook) (message "hello from foo"))
    (run-mode-hooks 'foo-mode-hook)
    )

  (defmacro defhook-body-monitor-begin (hook-sym func-sym)
    (message "defhook-body-monitor beginning: Running `%s' from hook `%s'." 
             func-sym hook-sym))
)
)
