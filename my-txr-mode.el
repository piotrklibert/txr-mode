(require 'rx)

(defconst txr-mode-quoted-string-re
  (rx
   (group
    (or  (char ?\")
         (char ?`))
    (minimal-match
     (zero-or-more
      (or (seq ?\\ ?\\)
          (seq ?\\ ?\")
          (seq ?\\ (not (any ?\" ?\\)))
          (not (any ?\" ?\\)))))
    (or (char ?\")
        (char ?`)))))


(defconst txr-mode-keywords
  (list "%e%" "%pi%" "*" "*args*"
        "*args-full*" "*e*" "*flo-dig*" "*flo-epsilon*"
        "*flo-max*" "*flo-min*" "*full-args*" "*gensym-counter*"
        "*keyword-package*" "*lib-version*" "*listener-hist-len*"
        "*listener-multi-line-p*" "*pi*" "*place-clobber-expander*"
        "*place-delete-expander*" "*place-macro*"
        "*place-update-expander*" "*random-state*" "*self-path*" "*stddebug*"
        "*stderr*" "*stdin*" "*stdlog*" "*stdnull*"
        "*stdout*" "*system-package*" "*txr-version*" "*unhandled-hook*"
        "*user-package*" "+" "-" "/"
        "/=" ":" ":append" ":args"
        ":atime" ":auto" ":blksize" ":blocks"
        ":cdigit" ":chars" ":continue" ":counter"
        ":cspace" ":ctime" ":cword-char" ":decline"
        ":dev" ":digit" ":downcase" ":env"
        ":equal-based" ":fd" ":filter" ":finish"
        ":from-current" ":from-end" ":from-start" ":from_html"
        ":frompercent" ":fromurl" ":fun" ":gap"
        ":gid" ":greedy" ":hextoint" ":ino"
        ":into" ":lfilt" ":lines" ":list"
        ":longest" ":maxgap" ":maxtimes" ":mingap"
        ":mintimes" ":mode" ":mtime" ":name"
        ":named" ":next-spec" ":nlink" ":nothrow"
        ":prio" ":rdev" ":real-time" ":repeat-spec"
        ":resolve" ":rfilt" ":shortest" ":size"
        ":space" ":string" ":symacro" ":times"
        ":to_html" ":tofloat" ":toint" ":tonumber"
        ":topercent" ":tourl" ":uid" ":upcase"
        ":var" ":vars" ":weak-keys" ":weak-vals"
        ":whole" ":word-char" "<" "<="
        "=" ">" ">=" "abort"
        "abs" "abs-path-p" "acons" "acons-new"
        "aconsql-new" "acos" "ado" "alist-nremove"
        "alist-remove" "all" "and" "andf"
        "ap" "apf" "append" "append*"
        "append-each" "append-each*" "apply" "aret"
        "ash" "asin" "assoc" "assql"
        "atan" "atan2" "atom" "bignump"
        "bindable" "bit" "block" "boundp"
        "break-str" "caaaaar" "caaaadr" "caaaar"
        "caaadar" "caaaddr" "caaadr" "caaar"
        "caadaar" "caadadr" "caadar" "caaddar"
        "caadddr" "caaddr" "caadr" "caar"
        "cadaaar" "cadaadr" "cadaar" "cadadar"
        "cadaddr" "cadadr" "cadar" "caddaar"
        "caddadr" "caddar" "cadddar" "caddddr"
        "cadddr" "caddr" "cadr" "call"
        "call-clobber-expander" "call-delete-expander" "call-update-expander"
        "callf" "car" "caseq" "caseql" "casequal"
        "cat-str" "cat-streams" "cat-vec" "catch"
        "catenated-stream-p" "catenated-stream-push" "cdaaaar" "cdaaadr"
        "cdaaar" "cdaadar" "cdaaddr" "cdaadr"
        "cdaar" "cdadaar" "cdadadr" "cdadar"
        "cdaddar" "cdadddr" "cdaddr" "cdadr"
        "cdar" "cddaaar" "cddaadr" "cddaar"
        "cddadar" "cddaddr" "cddadr" "cddar"
        "cdddaar" "cdddadr" "cdddar" "cddddar"
        "cdddddr" "cddddr" "cdddr" "cddr"
        "cdr" "ceil" "chain" "chand"
        "chdir" "chmod" "chr-isalnum" "chr-isalpha"
        "chr-isascii" "chr-isblank" "chr-iscntrl" "chr-isdigit"
        "chr-isgraph" "chr-islower" "chr-isprint" "chr-ispunct"
        "chr-isspace" "chr-isunisp" "chr-isupper" "chr-isxdigit"
        "chr-num" "chr-str" "chr-str-set" "chr-tolower"
        "chr-toupper" "chrp" "clamp" "clear-error"
        "close-stream" "closelog" "cmp-str" "collect-each"
        "collect-each*" "comb" "compl-span-str" "cond"
        "conda" "cons" "conses" "conses*"
        "consp" "constantp" "copy" "copy-alist"
        "copy-cons" "copy-hash" "copy-list" "copy-str"
        "copy-struct" "copy-vec" "cos" "count-if"
        "countq" "countql" "countqual" "cum-norm-dist"
        "daemon" "dec" "define-modify-macro" "define-place-macro"
        "defmacro" "defparm" "defparml" "defplace"
        "defstruct" "defsymacro" "defun" "defvar"
        "defvarl" "del" "delay" "delete-package"
        "display-width" "do" "dohash" "dotimes"
        "downcase-str" "dup" "dupfd" "dwim"
        "each" "each*" "eighth" "empty"
        "endgrent" "endpwent" "ensure-dir" "env"
        "env-fbind" "env-hash" "env-vbind" "eq"
        "eql" "equal" "errno" "error"
        "eval" "evenp" "exec" "exit"
        "exit*" "exp" "expt" "exptmod"
        "false" "fboundp" "fifth" "fileno"
        "filter-equal" "filter-string-tree" "finalize" "find"
        "find-if" "find-max" "find-min" "find-package"
        "find-struct-type" "first" "fixnump" "flatcar"
        "flatcar*" "flatten" "flatten*" "flet"
        "flip" "flipargs" "flo-dig" "flo-epsilon"
        "flo-int" "flo-max" "flo-min" "flo-str"
        "floatp" "floor" "flush-stream" "fmakunbound"
        "for" "for*" "force" "fork"
        "format" "fourth" "fstat" "fun"
        "func-get-env" "func-get-form" "func-get-name" "func-set-env"
        "functionp" "gcd" "gen" "generate"
        "gensym" "gequal" "get-byte" "get-char"
        "get-clobber-expander" "get-delete-expander" "get-error" "get-error-str"
        "get-hash-userdata" "get-indent" "get-indent-mode" "get-line"
        "get-lines" "get-list-from-stream" "get-sig-handler" "get-string"
        "get-string-from-stream" "get-update-expander" "getegid" "getenv"
        "geteuid" "getgid" "getgrent" "getgrgid"
        "getgrnam" "getgroups" "gethash" "getitimer"
        "getpid" "getppid" "getpwent" "getpwnam"
        "getpwuid" "getuid" "giterate" "glob"
        "glob-altdirfunc" "glob-brace" "glob-err" "glob-mark"
        "glob-nocheck" "glob-noescape" "glob-nomagic" "glob-nosort"
        "glob-onlydir" "glob-period" "glob-tilde" "glob-tilde-check"
        "greater" "group-by" "gun" "hash"
        "hash-alist" "hash-construct" "hash-count" "hash-diff"
        "hash-eql" "hash-equal" "hash-from-pairs" "hash-isec"
        "hash-keys" "hash-list" "hash-pairs" "hash-proper-subset"
        "hash-revget" "hash-subset" "hash-uni" "hash-update"
        "hash-update-1" "hash-values" "hashp" "have"
        "html-decode" "html-encode" "iapply" "identity"
        "ido" "if" "ifa" "iff"
        "iffi" "iflet" "ignerr" "in"
        "inc" "inc-indent" "indent-code" "indent-data"
        "indent-off" "inhash" "int-flo" "int-str"
        "integerp" "intern" "interp-fun-p" "interpose"
        "ip" "ipf" "isqrt" "itimer-prov"
        "itimer-real" "itimer-virtual" "juxt" "keep-if"
        "keep-if*" "keyword-package" "keywordp" "kill"
        "labels" "lambda" "last" "lazy-str"
        "lazy-str-force" "lazy-str-force-upto" "lazy-str-get-trailing-list" "lazy-stream-cons"
        "lazy-stringp" "lcm" "lcons" "lcons-fun"
        "lconsp" "ldiff" "length" "length-list"
        "length-str" "length-str-<" "length-str-<=" "length-str->"
        "length-str->=" "length-vec" "lequal" "less"
        "let" "let*" "lexical-fun-p" "lexical-lisp1-binding"
        "lexical-var-p" "lib-version" "link" "lisp-parse"
        "list" "list*" "list-str" "list-vector"
        "listp" "load" "log" "log-alert"
        "log-auth" "log-authpriv" "log-cons" "log-crit"
        "log-daemon" "log-debug" "log-emerg" "log-err"
        "log-info" "log-ndelay" "log-notice" "log-nowait"
        "log-odelay" "log-perror" "log-pid" "log-user"
        "log-warning" "log10" "log2" "logand"
        "logior" "lognot" "logtest" "logtrunc"
        "logxor" "lstat" "mac-param-bind" "macro-form-p"
        "macro-time" "macroexpand" "macroexpand-1" "macrolet"
        "major" "make-catenated-stream" "make-env" "make-hash"
        "make-lazy-cons" "make-like" "make-package" "make-random-state"
        "make-similar-hash" "make-string-byte-input-stream" "make-string-input-stream" "make-string-output-stream"
        "make-strlist-output-stream" "make-struct" "make-struct-type" "make-sym"
        "make-time" "make-time-utc" "make-trie" "makedev"
        "makunbound" "mapcar" "mapcar*" "mapdo"
        "mapf" "maphash" "mappend" "mappend*"
        "mask" "match-fun" "match-regex" "match-regex-right"
        "match-regst" "match-regst-right" "match-str" "match-str-tree"
        "max" "member" "member-if" "memq"
        "memql" "memqual" "merge" "meth"
        "method" "min" "minor" "minusp"
        "mkdir" "mknod" "mkstring" "mlet"
        "mod" "multi" "multi-sort" "n-choose-k"
        "n-perm-k" "nconc" "new" "nil"
        "nilf" "ninth" "none" "not"
        "notf" "nreverse" "nthcdr" "null"
        "nullify" "num-chr" "num-str" "numberp"
        "oand" "oddp" "op" "open-command"
        "open-directory" "open-file" "open-fileno" "open-files"
        "open-files*" "open-pipe" "open-process" "open-tail"
        "openlog" "opip" "or" "orf"
        "package-alist" "package-name" "package-symbols" "packagep"
        "pad" "partition" "partition*" "partition-by"
        "path-blkdev-p" "path-chrdev-p" "path-dir-p" "path-executable-to-me-p"
        "path-exists-p" "path-file-p" "path-mine-p" "path-my-group-p"
        "path-newer" "path-older" "path-pipe-p" "path-private-to-me-p"
        "path-same-object" "path-setgid-p" "path-setuid-p" "path-sock-p"
        "path-sticky-p" "path-symlink-p" "path-writable-to-me-p" "perm"
        "pipe" "place-form-p" "placelet" "placelet*"
        "plusp" "poll" "poll-err" "poll-in"
        "poll-nval" "poll-out" "poll-pri" "poll-rdband"
        "poll-wrband" "pop" "pos" "pos-if"
        "pos-max" "pos-min" "posq" "posql"
        "posqual" "pppred" "ppred" "pprinl"
        "pprint" "pprof" "pred" "prinl"
        "print" "prof" "prog1" "progn"
        "prop" "proper-listp" "pset" "push"
        "pushhash" "pushnew" "put-byte" "put-char"
        "put-line" "put-lines" "put-string" "put-strings"
        "pwd" "qquote" "qref" "quote"
        "raise" "rand" "random" "random-fixnum"
        "random-state-p" "range" "range*" "range-regex"
        "rcomb" "read" "readlink" "real-time-stream-p"
        "reduce-left" "reduce-right" "ref" "refset"
        "regex-compile" "regex-parse" "regexp" "regsub"
        "rehome-sym" "remhash" "remove-if" "remove-if*"
        "remove-path" "remq" "remq*" "remql"
        "remql*" "remqual" "remqual*" "rename-path"
        "repeat" "replace" "replace-list" "replace-str"
        "replace-vec" "rest" "ret" "retf"
        "return" "return-from" "reverse" "rlcp"
        "rlet" "rotate" "rperm" "rplaca"
        "rplacd" "run" "s-ifblk" "s-ifchr"
        "s-ifdir" "s-ififo" "s-iflnk" "s-ifmt"
        "s-ifreg" "s-ifsock" "s-irgrp" "s-iroth"
        "s-irusr" "s-irwxg" "s-irwxo" "s-irwxu"
        "s-isgid" "s-isuid" "s-isvtx" "s-iwgrp"
        "s-iwoth" "s-iwusr" "s-ixgrp" "s-ixoth"
        "s-ixusr" "search" "search-regex" "search-regst"
        "search-str" "search-str-tree" "second" "seek-stream"
        "select" "self-path" "seqp" "set"
        "set-diff" "set-hash-userdata" "set-indent" "set-indent-mode"
        "set-sig-handler" "setegid" "setenv" "seteuid"
        "setgid" "setgrent" "sethash" "setitimer"
        "setlogmask" "setpwent" "setuid" "seventh"
        "sh" "shift" "shuffle" "sig-abrt"
        "sig-alrm" "sig-bus" "sig-check" "sig-chld"
        "sig-cont" "sig-fpe" "sig-hup" "sig-ill"
        "sig-int" "sig-io" "sig-iot" "sig-kill"
        "sig-pipe" "sig-poll" "sig-prof" "sig-pwr"
        "sig-quit" "sig-segv" "sig-stkflt" "sig-stop"
        "sig-sys" "sig-term" "sig-trap" "sig-tstp"
        "sig-ttin" "sig-ttou" "sig-urg" "sig-usr1"
        "sig-usr2" "sig-vtalrm" "sig-winch" "sig-xcpu"
        "sig-xfsz" "sign-extend" "sin" "sixth"
        "size-vec" "slot" "slotset" "some"
        "sort" "sort-group" "source-loc" "source-loc-str"
        "span-str" "special-operator-p" "special-var-p" "splice"
        "split" "split-str" "split-str-set" "sqrt"
        "sssucc" "ssucc" "stat" "stdlib"
        "str<" "str<=" "str=" "str>"
        "str>=" "stream-get-prop" "stream-set-prop" "streamp"
        "string-extend" "string-lt" "stringp" "struct-type"
        "struct-type-p" "structp" "sub" "sub-list"
        "sub-str" "sub-vec" "subtypep" "succ"
        "super" "swap" "symacrolet" "symbol-function"
        "symbol-name" "symbol-package" "symbol-value" "symbolp"
        "symlink" "sys:*lisp1*" "sys:do-path-test" "sys:dwim-del"
        "sys:dwim-set" "sys:eval-err" "sys:expand" "sys:expr"
        "sys:fbind" "sys:gc" "sys:gc-set-delta" "sys:get-fb"
        "sys:get-vb" "sys:l1-setq" "sys:l1-val" "sys:lbind"
        "sys:lisp1-setq" "sys:lisp1-value" "sys:load" "sys:mark-special"
        "sys:path-access" "sys:path-examine" "sys:path-test" "sys:path-test-mode"
        "sys:pl-expand" "sys:placelet-1" "sys:qquote" "sys:quasi"
        "sys:quasilist" "sys:rplaca" "sys:rplacd" "sys:setq"
        "sys:setqf" "sys:splice" "sys:struct-lit" "sys:sym-clobber-expander"
        "sys:sym-delete-expander" "sys:sym-update-expander" "sys:top-fb" "sys:top-vb"
        "sys:trigger-load" "sys:unquote" "sys:var" "sys:with-saved-vars"
        "syslog" "system-package" "t" "tan"
        "tb" "tc" "tenth" "test-set-indent-mode"
        "tf" "third" "throw" "throwf"
        "time" "time-fields-local" "time-fields-utc" "time-string-local"
        "time-string-utc" "time-struct-local" "time-struct-utc" "time-usec"
        "tofloat" "toint" "tok-str" "tok-where"
        "tostring" "tostringp" "tprint" "transpose"
        "tree-bind" "tree-case" "tree-find" "trie-add"
        "trie-compress" "trie-lookup-begin" "trie-lookup-feed-char" "trie-value-at"
        "trim-str" "true" "trunc" "trunc-rem"
        "tuples" "txr-case" "txr-case-impl" "txr-if"
        "txr-version" "txr-when" "typeof" "typep"
        "unget-byte" "unget-char" "uniq" "unique"
        "unless" "unquote" "unsetenv" "until"
        "until*" "unwind-protect" "upcase-str" "update"
        "url-decode" "url-encode" "user-package" "usleep"
        "vec" "vec-push" "vec-set-length" "vecref"
        "vector" "vector-list" "vectorp" "w-continued"
        "w-coredump" "w-exitstatus" "w-ifcontinued" "w-ifexited"
        "w-ifsignaled" "w-ifstopped" "w-nohang" "w-stopsig"
        "w-termsig" "w-untraced" "wait" "weave"
        "when" "whenlet" "where" "while"
        "while*" "whilet" "width" "width-check"
        "with-clobber-expander" "with-delete-expander" "with-gensyms" "with-resources"
        "with-update-expander" "wrap" "wrap*" "zap"
        "zerop" "zip"))

(defconst txr-mode-keywords-re
  (concat
   "\\(" (s-join "\\|" (--map (concat "(\\<" it "\\>") txr-mode-keywords)) "\\)"))


(defconst txr-mode-comment-re
  (rx
   (group
    (seq "@;"
         (minimal-match
          (one-or-more any))
         line-end))))


(defconst txr-mode-function-re
  (rx
   (seq
    (or "\(" )
    (group
     (minimal-match
      (one-or-more (not whitespace))))
    (or whitespace ")"))))


(defconst txr-mode-var-re
  (rx
   (seq "@" (optional "{")
        (group (minimal-match
                (one-or-more alphanumeric)))
        (or "}" eow))))


(defconst txr-mode-kwargs-re
  (rx
   (group
    ":"
    (one-or-more alphanumeric))))


(defconst txr-font-lock-keywords
  (list
   (list txr-mode-quoted-string-re   1 font-lock-string-face)
   (list txr-mode-var-re             1 font-lock-constant-face)
   (list txr-mode-comment-re         1 font-lock-comment-face)
   (list txr-mode-function-re        1 font-lock-keyword-face)
   (list txr-mode-keywords-re        1 font-lock-builtin-face)
   (list txr-mode-kwargs-re          1 font-lock-constant-face)))


(define-derived-mode txr-mode lisp-mode "TXR"
  "Major mode for editing JSON files"
  (set
   (make-local-variable 'font-lock-defaults) '(txr-font-lock-keywords t)))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.txr$" . txr-mode))

(provide 'txr-mode)
;;; txr-mode.el ends here
