package matmul

import scala.math.log
import scala.math.round

import chisel3._
import chisel3.util._


class MyFloatOpp extends Module {
  val io = IO(new Bundle {
    val opp = Input(UInt(1.W))
    val in0 = Input(UInt(32.W))
    val in1 = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })

  // Add two floats
  def add_float(sign_a: UInt, exp_a: UInt, mant_a: UInt,
                sign_b: UInt, exp_b: UInt, mant_b: UInt): UInt = {
    val res = Wire(UInt(32.W))

    // Check for special cases (Infinity, Nan or 0)
    when (((exp_a === 255.U) && (mant_a =/= 0.U)) || ((exp_b === 255.U) && (mant_b =/= 0.U))) {
      // Raise NaN exception
      printf("NaN detected in addition\n")
      // Return NaN
      res := "b01111111111111111111111111111111".U

    } .elsewhen ((exp_a === 255.U) || (exp_b === 255.U)) {
      // Raise Infinity exception
      printf("Infinity detected in addition\n")
      // If inf + -inf, result is NaN
      when ((exp_a === 255.U) && (exp_b === 255.U) && ((sign_a + sign_b) === 2.U)) {
        res := "b01111111111111111111111111111111".U
      // Otherwise result is inf (with same sign as input that is inf)
      } .otherwise {
        res := Mux(exp_a === 255.U, Cat(sign_a, exp_a, mant_a), Cat(sign_b, exp_b, mant_b))
      } 
      
    } .elsewhen ((Cat(exp_a, mant_a) === 0.U) || (Cat(exp_a, mant_a) === 0.U)) {
      // Zero in addition
      printf("Zero detected in addition\n")
      // If zero + zero, sign is +ve unless both negative signs
      when (Cat(exp_a, mant_a, exp_b, mant_b) === 0.U) {
        res := Cat(sign_a&sign_b, exp_a, mant_a)
      // Otherwise, res = non zero input
      } .otherwise {
        res := Mux(exp_a === 0.U, Cat(sign_b, exp_b, mant_b), Cat(sign_a, exp_a, mant_a))
      }

    } .otherwise {
      // General addition
      printf("General addition\n")
      val frac_a         = Wire(UInt((1+23+3).W)) // 27
      val frac_b         = Wire(UInt((1+23+3).W)) // 27
      val frac_a_s       = Wire(UInt((1+23+3).W)) // 27
      val frac_b_s       = Wire(UInt((1+23+3).W)) // 27
      val frac_a_s_out   = Wire(UInt(1.W))
      val frac_b_s_out   = Wire(UInt(1.W))
      val frac_sum       = Wire(UInt((1+(1+23+3)).W)) // 28
      val frac_sum_s     = Wire(UInt((1+(1+23+3)).W)) // 28
      val frac_sum_s_out = Wire(UInt(1.W))
      val frac_sum_rnd   = Wire(UInt((1+23).W))
      val frac_sum_fin   = Wire(UInt(23.W))
      val exp_res        = Wire(UInt(8.W))
      val exp_res_c      = Wire(UInt(8.W))
      val exp_res_rnd    = Wire(UInt(8.W))
      val exp_res_fin    = Wire(UInt(8.W))
      val diff_exp       = Wire(UInt(8.W))
      val sign_res       = Wire(UInt(1.W))
      val shift_bit      = Wire(UInt(1.W))
      val shift_out      = Wire(UInt((1+23+3).W)) // 27  
      val rnd_bits       = Wire(UInt(3.W))
      val lead_zeros     = Wire(UInt(5.W))
      val lead_zeros_sub = Wire(UInt(5.W))
      
      // Initialisations (Set to 0 as they may not be used in calculation):
      diff_exp := 0.U
      frac_sum := 0.U
      frac_sum_rnd := 0.U
      exp_res_rnd := 0.U
      shift_bit := 0.U
      shift_out := 0.U
      lead_zeros := 0.U
      lead_zeros_sub := 0.U

      // Add leading one to mantissa (if exp not zero) and add 3 zero bits (for rounding)
      val three_zeros = Wire(UInt(3.W))
      three_zeros := 0.U
      frac_a := Mux(exp_a =/= 0.U, Cat("b1".U, mant_a, three_zeros), Cat("b0".U, mant_a, three_zeros))
      frac_b := Mux(exp_b =/= 0.U, Cat("b1".U, mant_b, three_zeros), Cat("b0".U, mant_b, three_zeros))
      // Match exponenets
      when (exp_a > exp_b) {
        // Exponent of a is bigger => adjust b fraction, keep a fraction same    
        // If b is denormalised, find amount of leading zeros from fraction
        lead_zeros := PriorityEncoder(Reverse(frac_b))    
        diff_exp := exp_a - exp_b + lead_zeros
        frac_b_s := frac_b >> diff_exp
        frac_a_s := frac_a
        // Set exponent for output
        exp_res := exp_a
        // If any non-zero bits shifted out, make last bit of frac 1 (skip if all bits shifted out)
        shift_out := frac_b << (27.U - diff_exp)
        frac_b_s_out := Mux(((diff_exp > 3.U) && (diff_exp < 27.U)) && (shift_out > 0.U), "b1".U, frac_b_s(0))
        frac_a_s_out := frac_a_s(0)

      // Exponent of b is bigger
      } .elsewhen (exp_a < exp_b) {
        lead_zeros := PriorityEncoder(Reverse(frac_a))  
        diff_exp := exp_b - exp_a + lead_zeros
        frac_a_s := frac_a >> diff_exp
        frac_b_s := frac_b
        exp_res := exp_b
        shift_out := frac_a << (27.U - diff_exp)
        frac_a_s_out := Mux(((diff_exp > 3.U) && (diff_exp < 27.U)) && (shift_out > 0.U), "b1".U, frac_a_s(0))
        frac_b_s_out := frac_b_s(0)

      // Exponents are the same
      } .otherwise {
        frac_a_s := frac_a
        frac_b_s := frac_b
        exp_res := exp_a
        frac_a_s_out := frac_a_s(0)
        frac_b_s_out := frac_b_s(0)
      }

      // Add fractions
      when (sign_a === sign_b) {
        // Same sign of operands means output must be same sign
        sign_res := sign_a
        // Add fractions (signs doesn't matter as they are the same)
        frac_sum := Cat("b0".U, frac_a_s(26,1), frac_a_s_out) + Cat("b0".U, frac_b_s(26,1), frac_b_s_out)
        // If there is a carry bit from frac_sum, increase exponent and shift frac_sum to right
        when (frac_sum(27) === 1.U) {
          exp_res_c := exp_res + 1.U
          // Check for overflow
          when (exp_res_c === 255.U) {
            frac_sum_s := 0.U
            frac_sum_s_out := 0.U
          } .otherwise {
            shift_bit := frac_sum(0)
            frac_sum_s := frac_sum >> 1.U          
            // If shifted out bit was 1, set last bit of frac_sum to 1
            frac_sum_s_out := Mux(shift_bit.asBool, "b1".U, frac_sum_s(0))
          }
          
        // Check if expected denormalised result is contradicted by sum being too large now
        } .elsewhen ((exp_res === 0.U) && (frac_sum(26) === 1.U)) {
          frac_sum_s := frac_sum
          frac_sum_s_out := frac_sum_s(0)
          exp_res_c := 1.U

        } .otherwise {
          // If no carry bit from sum, keep sum and exponent same
          frac_sum_s := frac_sum
          frac_sum_s_out := frac_sum_s(0)
          exp_res_c := exp_res
        }

      // Subtract fractions
      } .otherwise {
        // Input b has larger fraction
        when (Cat(frac_b_s, frac_b_s_out) > Cat(frac_a_s, frac_a_s_out)) {
          // As b fraction larger than a fraction, sign of result is sign of b
          sign_res := sign_b
          // Want to subtract from larger fraction
          frac_sum := Cat("b0".U, frac_b_s(26,1), frac_b_s_out) - Cat("b0".U, frac_a_s(26,1), frac_a_s_out)
          // If denormalised result expected, no need to modify exponent or fraction
          when (exp_res === 0.U) {
            exp_res_c := exp_res
            frac_sum_s := frac_sum

          // Else, subtract amount of leading zeros from exponent and shift fraction to left by same amount
          } .otherwise {
            // Find amount of leading zeros from subtraction
            lead_zeros_sub := PriorityEncoder(Reverse(frac_sum(26,3))) 
            exp_res_c := exp_res - lead_zeros_sub
            frac_sum_s := frac_sum << lead_zeros_sub
          }      
          frac_sum_s_out := frac_sum_s(0)    
        
        // Input a has larger fraction
        } .elsewhen (Cat(frac_a_s, frac_a_s_out) > Cat(frac_b_s, frac_b_s_out)) {
          sign_res := sign_a
          frac_sum := Cat("b0".U, frac_a_s(26,1), frac_a_s_out) - Cat("b0".U, frac_b_s(26,1), frac_b_s_out)
          when (exp_res === 0.U) {
            exp_res_c := exp_res
            frac_sum_s := frac_sum
          } .otherwise {
            lead_zeros_sub := PriorityEncoder(Reverse(frac_sum(26,3))) 
            exp_res_c := exp_res - lead_zeros_sub
            frac_sum_s := frac_sum << lead_zeros_sub
          }
          frac_sum_s_out := frac_sum_s(0)

        // Same fraction and different sign, so result is zero
        } .otherwise {
          sign_res := 0.U
          frac_sum_s := 0.U
          frac_sum_s_out := 0.U
          exp_res_c := 0.U
        }
      }

      // Extract three rounding bits from fraction
      rnd_bits := Cat(frac_sum_s(2,1), frac_sum_s_out)
      // Round up if last bit of fraction (w/o round_bits) is 0 and round_bits >= 101 or if last bit is 1 and round_bits >= 100
      when (((frac_sum_s(3) === 0.U) && (rnd_bits >= "b101".U)) || ((frac_sum_s(3) === 1.U) && (rnd_bits >= "b100".U))) { 
        frac_sum_rnd := Cat("b0".U, frac_sum_s(25, 3)) + 1.U
        // Check if rounding up led to a carry bit
        when (frac_sum_rnd(23) === 1.U) {
          // If carry bit, increase exponent and shift fraction right by 1
          exp_res_rnd := exp_res_c + 1.U
          // If exponent is too large now (has overflowed to 0), result is infinity
          when (exp_res_rnd === 0.U) {
            exp_res_fin := 255.U
            frac_sum_fin := 0.U
          } .otherwise {
            exp_res_fin := exp_res_rnd
            frac_sum_fin := frac_sum_rnd(22, 0)
          }

        } .otherwise {
          // Check if denormalised result needs to become normalised due to carry bit from rounding
          when ((exp_res_c === 0.U) && (frac_sum_rnd(22) === 1.U)) {
            exp_res_fin := 1.U
            frac_sum_fin := frac_sum_rnd(22, 0)
          } .otherwise {
            exp_res_fin := exp_res_c
            frac_sum_fin := frac_sum_rnd(22, 0)
          }
        }

      // No rounding required
      } .otherwise {
        frac_sum_fin := frac_sum_s(25, 3)
        exp_res_fin := exp_res_c
      }

      // Return output of addition
      res := Cat(sign_res, exp_res_fin, frac_sum_fin)
    }
    return res
  }



  // Multiply two floats
  def mult_float(sign_a: UInt, exp_a: UInt, mant_a: UInt,
                sign_b: UInt, exp_b: UInt, mant_b: UInt): UInt = {
    val res = Wire(UInt(32.W))

    // Check for special cases (Infinity, Nan or 0) (Cat(exp_a, mant_a) === 2139095040.U) || (Cat(exp_b, mant_b) === 2139095040.U)
    when (((exp_a === 255.U) && (mant_a =/= 0.U)) || ((exp_b === 255.U) && (mant_b =/= 0.U))) {
      // Raise NaN exception
      printf("NaN detected in multiplication\n")
      // Return NaN
      res := "b01111111111111111111111111111111".U

    } .elsewhen ((exp_a === 255.U) || (exp_b === 255.U)) {
      // Raise Infinity exception
      printf("Infinity detected in multiplication\n")
      // If Inf*0, return NaN. Else return (+/-) Inf (depeneding on signs of input)
      res := Mux(((exp_a === 255.U) && (Cat(exp_b, mant_b) === 0.U)) || ((exp_b === 255.U) && (Cat(exp_a, mant_a) === 0.U)), 
                  "b01111111111111111111111111111111".U, Cat(sign_a^sign_b, "b1111111100000000000000000000000".U))
      
    } .elsewhen ((Cat(exp_a, mant_a) === 0.U) || (Cat(exp_a, mant_a) === 0.U)) {
      // Zero multiplication
      printf("Zero detected in multiplication\n")
      val zero_res_wo_sign = Wire(UInt(31.W))
      zero_res_wo_sign := 0.U
      res := Cat(sign_a^sign_b, zero_res_wo_sign)

    } .otherwise {
      // General multiplication
      printf("General multiplication\n")
      val exp_sum        = Wire(UInt((1+8).W))
      val exp_sum_b      = Wire(UInt((1+8).W))
      val exp_sum_s      = Wire(UInt((1+8).W))
      val exp_sum_rnd    = Wire(UInt((1+8).W))
      val frac_a         = Wire(UInt((1+23).W))
      val frac_b         = Wire(UInt((1+23).W))
      val frac_mul       = Wire(UInt((2*(1+23)).W)) // 48
      val frac_mul_s     = Wire(UInt((2*(1+23)).W)) // 48
      val frac_mul_s_out = Wire(UInt(1.W))
      val frac_mul_rnd   = Wire(UInt((1+1+23).W)) // 25
      val frac_mul_rnd_s = Wire(UInt((1+1+23).W)) // 25
      val shift_bit      = Wire(UInt(1.W))
      val lead_zeros     = Wire(UInt(6.W))
      val underflow      = Wire(Bool())
      val overflow       = Wire(Bool())
      val denorm_res     = Wire(Bool())
      val denorm_res_2   = Wire(Bool())
      val renorm_res     = Wire(Bool())
      val renorm_res_2   = Wire(Bool())

      // Initialisations
      shift_bit := 0.U
      lead_zeros := 0.U
      frac_mul_rnd := 0.U
      renorm_res := false.B
      renorm_res_2 := false.B
      denorm_res := false.B
      denorm_res_2 := false.B
      underflow := false.B
      overflow := false.B

      // Add leading one to mantissa (if exp not zero)
      frac_a := Mux(exp_a =/= 0.U, Cat("b1".U, mant_a), Cat("b0".U, mant_a))
      frac_b := Mux(exp_b =/= 0.U, Cat("b1".U, mant_b), Cat("b0".U, mant_b))
      // Multiply fractions
      frac_mul := frac_a*frac_b
      // Add exponents
      exp_sum_b := Cat("b0".U, exp_a) + Cat("b0".U, exp_b)

      // Check if dealing with denormalised numbers
      when ((exp_a === 0.U) || (exp_b === 0.U)) {
        // If both denormalised, underflow occurs
        when (Cat(exp_a, exp_b) === 0.U) {
          underflow := true.B
        // Otherwise, determine which number is denormalised
        } .otherwise {
          // Set sum to opposite exponent + 1 as we want to subtract 126 instead of 127 (bias) now
          exp_sum_b := Mux(exp_a === 0.U, exp_b + 1.U, exp_a + 1.U)
        }
      }

      // If sum of exponents is less than or equal to bias (127), underflow may have occurred
      when (exp_sum_b <= 127.U) {
        // When sum is less than 102, underflow will always occur
        when (exp_sum_b <= 102.U) {
          underflow := true.B
        } .otherwise {
        // Otherwise, result is expected to be a denormalised number
          denorm_res := true.B
        }

      // If sum is greater than 3*bias (381), overflow has occurred
      } .elsewhen (exp_sum_b > 381.U) {
        overflow := true.B
      }
      
      // Subtract bias from sum of exponents
      exp_sum := exp_sum_b - 127.U

      // Normalise the result
      // If msb of mult is 1, shift frac to right and increase exponent (skip if expect denormalised result)
      when ((frac_mul(47) === 1.U) && !denorm_res) {
        shift_bit := frac_mul(0)
        frac_mul_s := frac_mul >> 1.U
        exp_sum_s := exp_sum + 1.U
        // Check if overflow of exponent has occurred
        when (exp_sum_s > 254.U) {
          overflow := true.B
        }
        // If shifted out bit was 1, set last bit of fraction to 1
        frac_mul_s_out := Mux(shift_bit.asBool, "b1".U, frac_mul_s(0))

      // Else, if 2nd msb is 1, no need to normalise
      } .elsewhen (frac_mul(46) === 1.U) {
        frac_mul_s := frac_mul
        exp_sum_s := exp_sum
        frac_mul_s_out := frac_mul_s(0)

      // Otherwise, need to shift fraction left and decrease exponent until normalised
      } .otherwise {
        // Check is result is expected to be denormalised
        when (!denorm_res) {
          // Find amount of leading zeros
          lead_zeros := PriorityEncoder(Reverse(frac_mul)) - 1.U
          // Check if decreasing exponent by lead_zeros is possible -> if not, denormalised result
          when (exp_sum <= lead_zeros) {
            denorm_res_2 := true.B
            // Shift fraction to the left by exponent sum - 1
            frac_mul_s := frac_mul << (exp_sum - 1.U)
            exp_sum_s := exp_sum
            frac_mul_s_out := frac_mul_s(0)

          // Else continue to normalise
          } .otherwise {
            // Shift fraction to left by lead_zeros
            frac_mul_s := frac_mul << lead_zeros
            // Decrease exponent to match fraction change
            exp_sum_s := exp_sum - lead_zeros
            // Check if decreasing exponent leads to underflow
            when (exp_sum < lead_zeros) {
              underflow := true.B
            }
            frac_mul_s_out := frac_mul_s(0)
          }

        // If already known to be denormalised, don't normalise
        } .otherwise {
          // Shift fraction to the left by the unbiased exponent *(-1) of the normalised input
          frac_mul_s := frac_mul >> (128.U - exp_sum_b)
          // Check if now need to normalise
          when (frac_mul_s(46) === 1.U) {
            renorm_res := true.B
            exp_sum_s := 1.U
            frac_mul_s_out := frac_mul_s(0)
          } .otherwise {
            exp_sum_s := exp_sum
            frac_mul_s_out := frac_mul_s(0)
          }
        }
      }

      // Rounding:
      // Round up if last bit of fraction (w/o round_bits) is 0 and round_bits >= 101 or if last bit is 1 and round_bits >= 100
      when (((frac_mul_s(23) === 0.U) && (frac_mul_s(22,20) >= "b101".U)) || ((frac_mul_s(23) === 1.U) && (frac_mul_s(22,20) >= "b100".U))) { 
        frac_mul_rnd := Cat("b0".U, frac_mul_s(46, 23)) + 1.U
        // Check if rounding up led to a carry bit
        when (frac_mul_rnd(24) === 1.U) {
          // If carry bit, increase exponent and shift fraction right by 1
          exp_sum_rnd := exp_sum_s + 1.U
          frac_mul_rnd_s := frac_mul_rnd >> 1.U
          // If exponent is too large now, overflow occurred
          when (exp_sum_rnd(8) === 1.U) {
            overflow := true.B
          } 
          
        } .otherwise {
          // Check if denormalised result needs to become normalised due to carry bit from rounding
          when ((denorm_res || denorm_res_2) && !renorm_res && (frac_mul_rnd(23) === 1.U)) {
            renorm_res_2 := true.B 
            exp_sum_rnd := 1.U
            frac_mul_rnd_s := frac_mul_rnd
          } .otherwise {
            exp_sum_rnd := exp_sum_s
            frac_mul_rnd_s := frac_mul_rnd
          }
        }

      // No rounding required
      } .otherwise {
        frac_mul_rnd_s := Cat("b0".U, frac_mul_s(46, 23))
        exp_sum_rnd := exp_sum_s
      }

      // If overflow, result is (+/-) Inf
      when (overflow === 1.U) {
        res := Cat(sign_a^sign_b, "b1111111100000000000000000000000".U)

      // If underflow, result is (+/-) 0
      } .elsewhen(underflow === 1.U) {
        val zero_res_wo_sign = Wire(UInt(31.W))
        zero_res_wo_sign := 0.U
        res := Cat(sign_a^sign_b, zero_res_wo_sign)

      // If result is denormalised, exponent is 0
      } .elsewhen ((denorm_res || denorm_res_2) && !(renorm_res || renorm_res_2)) {
        val zero_exp = Wire(UInt(8.W))
        zero_exp := 0.U
        res := Cat(sign_a^sign_b, zero_exp, frac_mul_rnd_s(22,0))

      // Otherwise, result is unchanged
      } .otherwise {
        res := Cat(sign_a^sign_b, exp_sum_rnd(7,0), frac_mul_rnd_s(22,0))
      }
    }
    
    return res
  }

  // Extract info about floating Point inputs
  val sign_a = Wire(UInt(1.W))
  val sign_b = Wire(UInt(1.W))
  val exp_a = Wire(UInt(8.W))
  val exp_b = Wire(UInt(8.W))
  val mant_a = Wire(UInt(23.W))
  val mant_b = Wire(UInt(23.W))

  sign_a := io.in0(31)
  sign_b := io.in1(31)
  exp_a := io.in0(30,23)
  exp_b := io.in1(30,23)
  mant_a := io.in0(22,0)
  mant_b := io.in1(22,0)

  // Determine which opeation to use (0 for add, 1 for mult)
  when (io.opp === 0.U) {
    io.out := add_float(sign_a, exp_a, mant_a, sign_b, exp_b, mant_b)
  } .otherwise {
    io.out := mult_float(sign_a, exp_a, mant_a, sign_b, exp_b, mant_b)
  }
  
  printf("Result = %b\n", io.out)

}

