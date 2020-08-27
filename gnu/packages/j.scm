;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Joseph Novakovich <josephnovakovich@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages j)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) :select (gpl3))
  #:use-module (gnu packages bash)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages readline))

(define-public j
  (package
    (name "j")
    (version "902")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/jsource")
         (commit "59324abbc6c9c3709d39096f5a41e0a4ef28e9f6")))
       (sha256
        (base32 "031mncgbnn89s9k34aml7jn9pr3cfmvfm06v8785br483jmdmn20"))))
    (build-system gnu-build-system)
    (inputs
     `(("bash" ,bash)
       ("readline" ,readline)
       ("bc" ,bc)
       ("libedit" ,libedit)
       ("pcre2" ,pcre2)
       ("zlib" ,zlib)))
    (outputs '("out"))
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((jplatform "linux")
                    (out (assoc-ref %outputs "out")))
               (with-output-to-file "jsrc/jversion.h"
                 (lambda ()
                   (display "#define jversion  ") (write ,version)  (newline)
                   (display "#define jplatform ") (write jplatform) (newline)
                   (display "#define jtype     ") (write "beta")    (newline)
                   (display "#define jlicense  ") (write "GPL3")    (newline)
                   (display "#define jbuilder  ") (write "guix.gnu.org")
                   (newline)))
               (substitute* `("jlibrary/system/main/regex.ijs")
                 (("pcre2dll=: f")
                  (string-append "pcre2dll=: '"
                                 (assoc-ref %build-inputs "pcre2")
                                 "/lib/libpcre2-8.so.0'")))
               (substitute* `("jlibrary/system/util/tar.ijs")
                 (("libz=: .+$")
                  (string-append "zlib=: '"
                                 (assoc-ref %build-inputs "zlib")
                                 "/lib/libz.so'\n")))
               #t)))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((jplatform "linux")
                   (j64x "j64avx2"))
               (chdir "make2")
               (system
                (format #f
                        "jplatform=~a j64x=~a USE_SLEEF=1 ./build_jconsole.sh"
                        jplatform j64x))
               (system
                (format #f
                        "jplatform=~a j64x=~a USE_SLEEF=1 ./build_tsdll.sh"
                        jplatform j64x))
               (system
                (format #f
                        "jplatform=~a j64x=~a USE_SLEEF=1 ./build_libj.sh"
                        jplatform
                        j64x))
               (chdir "..")
               #t)))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((jplatform "linux")
                    (j64x "j64avx2")
                    (tsu (string-append (getcwd) "/test/tsu.ijs"))
                    (jbld (string-append "bin/" jplatform "/" j64x)))
               ; following instructions from make2/make.txt
               (copy-recursively jbld "jlibrary/bin")
               (chdir "jlibrary/bin")
               (system "echo \"RUN ddall\" | ./jconsole ../../test/tsu.ijs")
               (chdir "../..")
               #t)))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin-out (string-append (assoc-ref %outputs "out") "/bin"))
                    (share-out (string-append (assoc-ref %outputs "out")
                                              "/share/j"))
                    (jconsole "jlibrary/bin/jconsole")
                    (libj.so  "jlibrary/bin/libj.so"))
               (install-file jconsole bin-out)
               (install-file libj.so bin-out)
               (copy-recursively "jlibrary/addons"
                                 (string-append share-out "/addons"))
               (copy-recursively "jlibrary/system"
                                 (string-append share-out "/system"))
               ; custom profile.ijs to work with guix
               (with-output-to-file (string-append bin-out "/profile.ijs")
                 (lambda ()
                   (display
                    "NB. J profile
NB. JFE sets BINPATH_z_ and ARGV_z_

jpathsep_z_=: '/'&(('\\' I.@:= ])})
home=. 2!:5'HOME'
BINPATH_z_=: home,'/.guix-profile/bin/jconsole'

bin=. BINPATH
install=. home,'/.guix-profile/share/j'
addons=. install,'/addons'
system=. install,'/system'
tools=. install,'/tools'
isroot=. 0
userx=. '/j902-user'
user=. home,userx
break=. user,'/break'
config=. user,'/config'
snap=. user,'/snap'
temp=. user,'/temp'
ids=. ;:'addons bin break config home install snap system tools temp user'

SystemFolders_j_=: ids,.jpathsep@\".&.>ids

NB. used to create mutable j user directories for temp
NB. files/configuring jqt/projects and so on
md=. 3 : 0 NB. recursive makedir
a=. jpathsep y,'/'
if. -.#1!:0 }:a do.
  for_n. I. a='/' do. 1!:5 :: [ <n{.a end.
end.
)

NB. try to ensure user folders exist
md user,'/projects'
md break
md config
md snap
md temp

NB. boot up J and load startup.ijs if it exists
0!:0 <jpathsep (4!:55 (;:'isroot userx ids md'), ids)]system,'/util/boot.ijs'
")))
               #t))))))
    (synopsis "APL Dialect")
    (description "J is a programming language that works with arrays,
verbs, adverbs, and conjunctions.  For example, +/x sums array x and
/:~x sorts it.")
    (home-page "https://code.jsoftware.com/wiki/Main_Page")
    (license gpl3)))

