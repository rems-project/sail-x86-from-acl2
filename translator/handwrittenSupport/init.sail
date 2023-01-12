$ifndef __X86_INIT
$define __X86_INIT

// An extension hook for model initialization; called by main()
val initialize_model : unit -> unit
function initialize_model() = ()

$endif
