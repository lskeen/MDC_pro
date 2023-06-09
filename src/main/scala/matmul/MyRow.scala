package matmul

import chisel3._

class MyRow extends Bundle {
    val	a1_1	=	UInt(32.W)
    val	a1_2	=	UInt(32.W)
    val	a1_3	=	UInt(32.W)
    val	a1_4	=	UInt(32.W)
}