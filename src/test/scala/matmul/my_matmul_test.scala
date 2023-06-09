package matmul

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import scala.language.postfixOps

class MyMatmulTest extends AnyFreeSpec with ChiselScalatestTester {

    "Starting Test Case" in {
        test (new MyMatmul) { c =>

            // // TEST 2x2
            // c.io.row.poke("b0011111110000101000111101011100001000000001001110000101000111101".U)
            // c.io.col.poke("b0100000001000111101011100001010001000000000111001100110011001101".U)
            // c.clock.step(1)
            // c.io.result.expect("b01000001000110100011101010010010".U)

            // TEST 4x4
            c.io.row.poke("b01000000000010100011110101110001001111111000101000111101011100010100000100010010101110000101001001000000100100011110101110000101".U)
            c.io.col.poke("b00111111011000111101011100001010010000000101101000111101011100010100000011110100110011001100110101000001000000011110101110000101".U)
            c.io.result.expect("b01000010111000011001000011011000".U)

        

        }
    }
}