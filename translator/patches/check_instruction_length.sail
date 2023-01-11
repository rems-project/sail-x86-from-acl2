/*! Check if the length of an instruction exceeds 15 bytes.
<p> 
 The maximum length of an instruction is 15 bytes; 
 a longer instruction causes a #GP ( 0 ) exception. 
 See AMD manual, Dec ' 17, Volume 2, Table 8-6. 
 This function is used to check this condition. 
 </p> 
 <p> 
 The @ ( ' start-rip ' ) argument is 
 the instruction pointer at the beginning of the instruction. 
 The @ ( ' temp-rip ' ) argument is generally 
 the instruction pointer just past the end of the instruction, 
 in which case the @ ( ' delta-rip ' ) argument is 0. 
 In the other cases, @ ( ' delta-rip ' ) is a small non-zero number, 
 and @ ( ' temp-rip + delta-rip ' ) is 
 the instruction pointer just past the end of the instruction. 
 </p> 
 <p> 
 This function returns @ ( ' nil ' ) if the length does not exceed 15 bytes. 
 Otherwise, this function returns the offending length ( a number above 15 ) , 
 which is useful for error reporting in the model. 
 </p> */
val check_instruction_length : (sbits(64), sbits(64), bits(3)) -> option(int)
function check_instruction_length (start_rip, temp_rip, delta_rip) = {
  let end_rip : sbits(65) = sail_sign_extend(temp_rip, 65) + sail_zero_extend(delta_rip, 65);
  let length : int = signed(end_rip) - signed(start_rip);
  if (length <= 15 & canonical_address_p(signed(start_rip)) & canonical_address_p(signed(temp_rip)))
  then None()
  else Some(length)
}
