val bang_memi : forall 'addr_width, 'addr_width > 0. (bits('addr_width), bits(8)) -> unit
function bang_memi (addr, valToWrite) = {
/*	dummy = 
		__write_mem_ea(	Write_plain,				// Write type
						64,							// Address size
						sail_mask(64, addr),	// Address to write to (0 here)
						1);							// Number of bytes to write
*/
	dummy2 =
		__write_mem(Write_plain,					// Write type
					64,								// Address size
					sail_mask(64, addr),		// Address to write to (0 here)
					1,								// Number of bytes to write
					valToWrite	// Value to write
					);
	log_memory_write(addr, 1, valToWrite);
}
