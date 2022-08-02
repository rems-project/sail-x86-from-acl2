(include-book "projects/x86isa/tools/execution/top" :ttags :all :dir :system)
(in-package "X86ISA")
(binary-file-load "{}")
(init-x86-state-64 
 ;; Status (MS and fault field) 
 nil 
 ;; Start Address --- set the RIP to this address 
 {}
 ;; Initial values of General-Purpose Registers 
 '((#.*rsp* . #.*2^45*)) 
 ;; Control Registers: set cr4.osfxsr, needed for some instructions in the test suite
 '((4 . 512))
 ;; Model-Specific Registers: a value of nil will not nullify existing 
 ;; values. 
 nil 
 ;; Segment Registers: a value of nil will not nullify existing values. 
 nil ; visible portion 
 nil nil nil ; hidden portion 
 ;; Rflags Register 
 2 
 ;; Memory image: a value of nil will not nullify existing values. 
 nil 
 ;; x86 state 
 x86)
; (!log-file-name "logout.txt")
; (trace-all-reads)
; (trace-all-writes)
(log-instr-fn {} x86 state)
