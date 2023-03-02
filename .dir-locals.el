((nil
  (org-context-capture
   ("i" "@fixme" entry
    (file+headline "todo.org" "@ACTIVE")
    "** TODO [#A] %? :fix:\n   %a\n" )
   ("I" "@fixme (sub)" entry
    (file+headline "todo.org" "@ACTIVE")
    "** TODO [#A] %? [/] :fix:\n   %a\n   + [ ] ...\n\n")
   ("o" "@todo" entry
    (file+headline "todo.org" "@ACTIVE")
    "** TODO [#B] %?\n\n" )
   ("O" "@todo (sub)" entry
    (file+headline "todo.org" "@ACTIVE")
    "** TODO [#B] %? [/]\n   + [ ] ...\n\n")
   ("u" "@back" entry
    (file+headline "todo.org" "@BACKLOG")
    "** BACK [#B] %? \n\n")
   ("U" "@back (sub)" entry
    (file+headline "todo.org" "@BACKLOG")
    "** BACK [#B] %? [/]\n   + [ ] ...\n\n")
   ("e" "@test" entry
    (file "tests/test.org")
    "** TODO [#C] %? :test:\n   %a\n")
   ("E" "@test (sub)" entry
    (file "tests/test.org")
    "** TODO [#C] %? [/] :test:\n   %a\n   + [ ] ...\n\n")
   )
  (org-context-agenda
   ("o" "TODO + tests" ((alltodo "" ((org-agenda-files '("todo.org"))
                                     (org-agenda-overriding-header "@TODO")))
                        (alltodo "" ((org-agenda-overriding-header "@TESTS")
                                     (org-agenda-files '("tests/test.org")))))
    ((org-agenda-buffer-name "TODO: org-context"))))))
