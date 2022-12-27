"""
Low-level bit manipulation functions, nothing specific to ARM or any other architecture:

Do include:
  * Rotate and other bitwise functions that Python doesn't include
  * Bitfield setter and getter
  * Bit field description in strings
Don't include:
  * Functions that evaluate flags like ARM overflow (V) and negative (N)

Even though this is not specific to any particular architecture, we borrow terms
from specific architectures when neccesary. For instance:
  * xlen
      Bit length of a general-purpose register, borrowed from RISC-V. In
      general, when we refer to a processor as being 32-bit, 64-bit, 8-bit
      etc, we generalize to saying that it is xlen-bit, where xlen==32, 64,
      8, etc depending on the architecture. On ARMv4T (which is what I am
      focusing on first), xlen==32 only, so it doesn't need a definition
      in the ARM documentation. Since RISC-V was designed to be 32-bit and
      64-bit from the beginning, they needed and created a word, so we will
      use it.

      Default xlen is 32.
"""


def get_bits(val:int,b1:int,b0:int,xlen:int=32):
    """
    Reads a bitfield from a number (interpreted as an unsigned int of size xlen)
    :param val: value to pull bitfield from
    :param b1: high bit of bitfield
    :param b0: low bit of bitfield
    :param xlen: number of significant bits in val
    :return: bits of bitfield, shifted such that val[b0] is result[0]
    """
    xlen_mask=(1<<xlen)-1
    mask=(1<<(b1-b0+1))-1
    return ((val & xlen_mask) >> b0) & mask


def set_bits(val:int,bits:int,b1:int,b0:int,xlen:int=32):
    """
    Writes a bitfield to a number (interpreted as uint32_t)
    :param val: value to write bitfield to
    :param bits: bits of bitfield
    :param b1: high bit of bitfield
    :param b0: low bit of bitfield
    :param xlen: number of significant bits in val
    :return: val with the bits in the bitfield replaced by bits
    Note that bits is masked such that it will fit in the bitfield,
    IE set_bits(0x00,0,0,0xff)==0x01, not 0xff (since 0xff doesn't
    fit in 1 bit).
    Note that val is only an input argument, and its value is
    not affected. Only the least-significant xlen bits of val
    affect the result.
    """
    xlen_mask=(1<<xlen)-1
    mask=((1<<(b1-b0+1))-1)<<b0
    bits=(bits<<b0) & mask
    return ((val &~ mask) | bits) & xlen_mask


def sign_extend(signed:int, old_length:int, new_length:int)->int:
    """
    Sign extend a number. Given an old length and new length,
    extend the number to the new length, adding
    :param signed:
    :param old_length:
    :param new_length:
    :return:

    Note: Result is undefined if new_length<=old_length. This
    is intended to *extend* and I don't care what happens if
    the caller breaks this rule. It is likely to not do what
    the caller intended.
    """
    oldmask=(1<<old_length)-1
    oldbits=signed & oldmask
    sign=get_bits(oldbits,old_length-1,old_length-1)
    newbits=sign*(((1<<(new_length-old_length))-1)<<old_length)
    return oldbits | newbits


def rotate_right(val:int,ror:int,xlen:int=32):
    """
    Perform a rotate-right on an unsigned int.

    From Glossary-12:
    "Perform a right rotate, where each bit that is shifted off the right is inserted on the left"
    :param val: Value to rotate
    :param ror: Number of bits to rotate
    :param xlen: number of significant bits in val
    :return: Rotated value
    """
    xlen_mask=(1<<xlen)-1
    right_part=val>>ror
    left_part=(val<<(xlen-ror)) & xlen_mask
    return left_part | right_part


def arithmetic_shift_right(val:int,ror:int,xlen:int=32):
    """
    Perform a shift-right on a *signed* int. On an unsigned int, a
    shift right of n divides by 2**n, rounding down. Arithmetic
    shift right extends this to signed numbers. We want an
    arithmetic shift right to divide a negative number by 2**n
    as well, and remain negative. It turns out that this can be
    done in twos complement arithmetic by shifting 1 bits into
    the left end of a number instead of zero bits. To make a single
    operation that works for both positive and negative, we shift
    into the left end, copies of the original left-most bit (sign bit).
    This does the same thing as before to positive numbers, and
    the correct thing to negative numbers.

    From Glossary-1:
    "Performs a right shift, repeatedly inserting the original left-most bit (the sign bit) in
     the vacated bit positions on the left"
    :param val: Value to shift
    :param ror: Number of bits to shift
    :param xlen: length of words
    :return: shifter value

    """
    sign=get_bits(val,xlen-1,xlen-1)
    xlen_mask=(1<<xlen)-1
    right_part=(val & xlen_mask)>>ror
    left_part=(sign*((1<<ror)-1))<<(xlen-ror)
    return left_part | right_part


def bit_not(val:int,xlen:int=32):
    """
    Return the bitwise inverse. Need a special
    function because of Python's weird "infinite bit"
    integers.

    :param val:
    :param xlen:
    :return:
    :param xlen: number of significant bits in val
    """
    xlen_mask=(1<<xlen)-1
    return (~val) & xlen_mask


def bits_match(val:int,need0:int,need1:int):
    """
    Check if a bit pattern has 1s and 0s in required places.

    :param val: Value to check
    :param need0: Has a *1* bit in every position where val needs a *0* bit
    :param need1: Has a *1* bit in every position where val needs a *1* bit
    :return: True if the bit pattern matches, False otherwise
    """
    # Check 0 bits
    if (bit_not(val) & need0)!=need0:
        return False
    if (val & need1)!=need1:
        return False
    return True


def parse_bitpat(pat:str,xlen:int=32)->tuple[int,int,dict[str,tuple[int,int]]]:
    """
    Parses a string bit pattern. This string bit pattern
    is something that is simple to manually type from a
    reference.

    :param pat: String describing bit pattern
    :param xlen: Required length of bit pattern
    :return: Tuple:
      * Mask of bits that need to be 0 in order to match this pattern
      * Mask of bits that need to be 1 in order to match this pattern
      * A dictionary of tuples. The key is the character used in the
        string pattern, and the value is a tuple of (bit1,bit0) for
        the bitfield, suitable for passing to get_bits().

    Example:
    p=parse_bitpat('11110000aaaaaaaabbbbccccdddddddd')
    print(f'0x{p[0]:08x}')
      0x0F000000
    print(f'0x{p[1]:08x}')
      0xF0000000
    print(p[2])
      {'a':(23,16),'b':(15,12),'c':(11,8),'d':(7,0)}
    Note:
    * Any single character other than 1 and 0 can be used for
      a field description, but the convention is lower-case letters.
    * Bitfields are intended to be contiguous. It is undefined what
      happens if the same letter is used in two separated fields.
    """
    need1 = 0
    need0 = 0
    assert len(pat) == xlen, f'Wrong number of bits for pattern {pat}, should be {xlen}, is {len(pat)}'
    field_bit1 = {}
    field_bit0 = {}
    for i_bit, code in enumerate(reversed(pat)):
        if code == '0':
            need0 |= 1 << i_bit
        elif code == '1':
            need1 |= 1 << i_bit
        else:
            if code not in field_bit0:
                field_bit0[code] = i_bit
            field_bit1[code] = i_bit
    field={code:(b1,field_bit0[code]) for code,b1 in field_bit1.items()}
    return need0, need1, field


def decode_bitpat(val:int,fields:dict[str,tuple[int,int]])->dict[str,int]:
    """
    Decode a bit pattern. Given a value and a dictionary of fields (such
    as returned by parse_bitpat[2]), extract each field from the value
    using get_bits() and return a dictionary of the key and value of each
    field.

    :param val: Value
    :param fields: Dictionary of fields. Key is name of field (usually a
                   single character) and value is tuple of (bit1,bit0) of
                   the field, suitable for get_bits().
    :return: Dictionary of field values, one key for each key in fields.
    """
    field_vals = {}
    for code, (bit1, bit0) in fields.items():
        field_vals[code] = get_bits(val, bit1, bit0)
    return field_vals

