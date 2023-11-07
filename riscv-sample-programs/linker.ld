ENTRY(_start)

MEMORY{
	RAM (rwx) : ORIGIN = 0x80000000 + 1M, LENGTH = 255M
}

SECTIONS{
	.text :
	{
	    . = ALIGN(8);
	    PROVIDE(__TEXT_START_ = .);
		*(.text.init)
		*(.text*)
	    PROVIDE(__TEXT_END_ = .);
	} > RAM

	.data : {
		. = ALIGN(8);
		PROVIDE(__DATA_START_ = .);
    *(.data .data*)
    *(.sdata .sdata*)
    PROVIDE(__DATA_END_ = .);
    . = ALIGN(8);
    PROVIDE(__BINARY_END_ = .);
	} > RAM

	PROVIDE(__IO_ADDR_ = 0x10000000);

}