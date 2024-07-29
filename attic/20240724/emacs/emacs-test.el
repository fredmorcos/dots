(defun im/init (_name &rest entries)
 (let ((packages (or (plist-get entries :packages) '()))
       (definitions (or (plist-get entries :definitions) '()))
       (before (or (plist-get entries :before) '()))
       (after (or (plist-get entries :after) '()))
       (custom (or (plist-get entries :custom) '()))
       (hooks (or (plist-get entries :hooks) '())))
  (message (format "--> packages = %s" packages))
  (dolist (element after)
   (message (format "--> after    = %s" after))
   (let ((module (plist-get element :module))
         (action (plist-get element :do)))
    (message (format "  --> module = %s" module))
    (message (format "  --> action = %s" action)))))
 (dolist (element entries)
  (message (format "--> %s%s" element (if (keywordp element) " IS A KEYWORD!" "")))))

(im/init "Python"
 :packages '(python indent-guide)
 :after
 '((:module python :do (setq-local fill-column 80))))

(or (plist-get '(:foo foo :bar nil) :bar) t)
(plist-member '(:foo foo :bar nil) :bar)

(im/init "Python"
 :packages
 '(python indent-guide)
 :locals
 '(())
 :after
 '((:module python :local (fill-column 80))))
