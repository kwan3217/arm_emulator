from __future__ import annotations
from dataclasses import dataclass, field
from typing import Iterable

from bits import *


class UNPREDICTABLE(ValueError):
    pass


class UNDEFINED(ValueError):
    pass


def borrow_from(a:int,b:int)->int:
    """
    :param a: First operand of subtraction
    :param b: Second operand of subtraction
    :return: (from ARM glossary) Returns 1 if the subtraction specified
    as its parameter caused a borrow (the true result is less than 0, where
    the operands are treated as unsigned integers), and returns 0 in all
    other cases. This delivers further information about a subtraction which
    occurred earlier in the pseudo-code. The subtraction is not repeated.

    In the emulation, the subtraction (actually comparison) *is* repeated
    but this has no emulation-visible side-effects.
    """
    return 1 if (a & 0xFFFF_FFFF)<(b & 0xFFFF_FFFF) else 0


def overflow_from_add(a:int,b:int)->int:
    """

    :param a: First operand of addition
    :param b: Second operand of addition
    :return: (from ARM glossary OverflowFrom) Returns 1 if the addition...
    specified as its parameter caused a 32-bit signed overflow. Addition
    generates an overflow if both operands have the same sign (bit[31]),
    and the sign of the result is different to the sign of both operands.
    ...
    This delivers further information about an addition... which occurred
    earlier in the pseudo-code. The addition... is not repeated.
    """
    return 1 if (get_bits(a, 31, 31) == get_bits(b, 31, 31)) and (
                get_bits((a + b) & 0xFFFF_FFFF, 31, 31) != get_bits(a, 31, 31)) else 0


def overflow_from_sub(a:int,b:int)->int:
    """

    :param a: First operand of addition
    :param b: Second operand of addition
    :return: (from ARM glossary OverflowFrom)
    Returns 1 if the ...subtraction specified as its parameter caused a
    32-bit signed overflow.
    ...
    Subtraction causes an overflow if the operands have different signs,
    and the first operand and the result have different signs. This
    delivers further information about a...subtraction which occurred
    earlier in the pseudo-code. The ...subtraction is not repeated.
    """
    return 1 if (get_bits(a, 31, 31) != get_bits(b, 31, 31)) and (
                get_bits((a - b) & 0xFFFF_FFFF, 31, 31) != get_bits(a, 31, 31)) else 0


class Memory:
    def __init__(self,size:int):
        self.size=size
    def _decode_args(self,args):
        if type(args)==tuple:
            addr=args[0]
            if len(args)>1:
                n=args[1]
            else:
                n=4
        else:
            addr=args
            n=4
        return addr,n
    def __getitem__(self,args):
        """

        :param addr: Address relative to beginning of this memory's address space
        :param bytes: Number of bytes to get
        :return: An unsigned integer of the appropriate length, obtained by reading the
                 memory as little-endian.
        """
        addr,n=self._decode_args(args)
        result=0
        for i in range(n):
            result=result | (self.data[addr+i]<<(8*i))
        return result


class ROM(Memory):
    def __init__(self,infn,size,default=0xff):
        super().__init__(size)
        """

        :param infn: Binary image to load a the start of this ROM
        """
        with open(infn,'rb') as inf:
            self.data=inf.read(size)
            if len(self.data)<size:
                self.data=self.data+bytes([default])*(size-len(self.data))


class RAM(Memory):
    def __init__(self,size,default=0x00):
        super().__init__(size)
        self.data=bytearray([default])*size
    def __setitem__(self,args,value):
        """

        :param addr: Address relative to beginning of this memory's address space
        :param bytes: Number of bytes to get
        :return: An unsigned integer of the appropriate length, obtained by reading the
                 memory as little-endian.
        """
        addr,n=self._decode_args(args)
        for i in range(n):
            self.data[addr+i]= get_bits(value, i * 8 + 7, i * 8)


class AddressSpace(Memory):
    def __init__(self,segments:Iterable[tuple[int,Memory]]):
        super().__init__(0x1_0000_0000)
        self.segments={}
        for startaddr,segment in segments:
            self.install(startaddr,segment)
    def install(self,startaddr:int,mem:Memory):
        self.segments[startaddr]=mem
    def _getaddr(self,args):
        addr,n=self._decode_args(args)
        for startaddr,segment in self.segments.items():
            if addr>=startaddr and addr<(startaddr+segment.size):
                return segment,addr-startaddr,n
        raise ValueError(f'No memory mapped at address {addr:08x}')
    def __getitem__(self,args):
        segment,segaddr,n=self._getaddr(args)
        return segment[segaddr,n]
    def __setitem__(self,args,value):
        segment,segaddr,n=self._getaddr(args)
        segment[segaddr,n]=value


class xPSR:
    usr=0b10000 # (0x10, 16) User
    fiq=0b10001 # (0x11, 17) Fast Interrupt request
    irq=0b10010 # (0x12, 18) Normal Interrupt request
    svc=0b10011 # (0x13, 19) Supervisor
    abt=0b10111 # (0x17, 23) Abort (handles memory protection and virtual memory)
    und=0b11011 # (0x1B, 27) Undefined (supports software emulation of hardware coprocessors)
    sys=0b11111 # (0x1F, 31) sys System (runs in privileged mode)
    i_mode_map={0:usr,1:fiq,2:irq,3:svc,4:abt,5:und,6:sys}
    mode_map  ={mode:i_mode for i_mode,mode in i_mode_map.items()}
    mode_name_map={usr:'usr',fiq:'fiq',irq:'irq',svc:'svc',abt:'abt',und:'und',sys:'sys'}
    display_order=(usr,sys,svc,abt,und,irq,fiq)
    def __init__(self,val=0xc010_2ad0):
        self.val=val
    @property
    def M(self):
        return get_bits(self.val, 4, 0)
    @M.setter
    def M(self,val:int):
        self.val= set_bits(self.val, val, 4, 0)
    @property
    def N(self):
        """
        Negative flag -- bit 31 of result of the instruction. If this result is regarded
                         as a two's complement signed integer, then N=1 if the result is
                         negative and N=0 if it is positive or zero
        """
        return get_bits(self.val, 31, 31)
    @N.setter
    def N(self,val:int):
        self.val= set_bits(self.val, val, 31, 31)
    @property
    def Z(self):
        """
        Zero flag -- is set to 1 if the result of the instruction is zero (this often
                     indicates an *equal* result from a comparison), and to 0 otherwise.
        """
        return get_bits(self.val, 30, 30)
    @Z.setter
    def Z(self,val:int):
        self.val= set_bits(self.val, val, 30, 30)
    @property
    def C(self):
        """
        Carry Flag -- Is set in one of four ways:
                          * For an addition, including the comparison instruction CMN,
                            C is set to 1 if the addition produced a carry (that is,
                            an unsigned overflow), and to 0 otherwise.
                          * For a subtraction, including the comparison instruction CMP,
                            C is set to 0 if the subtraction produced a borrow (that is,
                            an unsigned underflow), and to 1 otherwise.
                          * For non-addition/subtractions that incorporate a shift
                            operation, C is set to the last bit shifted out of the value
                            by the shifter.
                          * For other non-addition/subtractions, C is normally left
                            unchanged (but see the individual instruction descriptions
                            for any special cases).
        """
        return get_bits(self.val, 29, 29)
    @C.setter
    def C(self,val:int):
        self.val= set_bits(self.val, val, 29, 29)
    @property
    def V(self):
        """
        Overflow flag -- Is set in one of two ways:
                             * For an addition or subtraction, V is set to 1 if signed
                               overflow occurred, regarding the operands and result as
                               two's complement signed integers.
                             * For non-addition/subtractions, V is normally left
                               unchanged (but see the individual instruction descriptions
                               for any special cases).
        """
        return get_bits(self.val, 28, 28)
    @V.setter
    def V(self,val:int):
        self.val= set_bits(self.val, val, 28, 28)
    @property
    def I(self):
        """
        Interrupt bit -- Disables IRQ interrupts when it is set.
        """
        return get_bits(self.val, 7, 7)
    @I.setter
    def I(self,val:int):
        self.val= set_bits(self.val, val, 7, 7)
    @property
    def F(self):
        """
        FIQ bit -- Disables FIQ interrupts when it is set
        """
        return get_bits(self.val, 6, 6)
    @F.setter
    def F(self,val:int):
        self.val= set_bits(self.val, val, 6, 6)
    @classmethod
    def str_psr(cls,psr:int)->str:
        bits=(31,30,29,28,7,6)
        names=('N','Z','C','V','I','F')
        result=''.join([n if get_bits(psr, bit, bit) else n.lower() for n, bit in zip(names, bits)])
        M= get_bits(psr, 4, 0)
        if M in cls.mode_name_map:
            result+=cls.mode_name_map[M]
        else:
            result+='!!!'
        return result
    def __str__(self):
        return f'{format_hex(self.val)} ({self.str_psr(self.val)})'
    def gpr_bank(self,rn):
        """
        Get the correct bank for the given gpr number, considering
        the cpsr mode
        :param rn: register number
        :return: bank index into self._gpr
        """
        if self.M in (self.usr,self.sys):
            return self.mode_map[self.usr]
        elif self.cpsr.M in (self.svc,self.abt,self.und,self.irq):
            if 13<=rn<=14:
                return self.mode_map[self.cpsr.M]
            else:
                return self.mode_map[self.usr]
        elif self.cpsr.M==self.fiq:
            if 8<=rn<=14:
                return self.mode_map[self.fiq]
            else:
                return self.mode_map[self.usr]
        else:
            raise ValueError("Invalid mode")
    def ConditionPassed(self,cond):
        # (f"if {cond_decode} ({cond_pass})" if cond_decode != 'AL' else '')
        if cond==0b000:
            cond_pass=1==self.Z
            return cond_pass,'eq',f"if equal [Z==1] ({cond_pass}) "
        elif cond==0b0001:
            cond_pass=0==self.Z
            return cond_pass,'ne',f"if not equal [Z==0] ({cond_pass}) "
        elif cond == 0b0010:
            cond_pass=1==self.C
            return cond_pass,'hs',f"if unsigned higher or same [C==1] ({cond_pass}) "
        elif cond == 0b0011:
            cond_pass=0==self.C
            return cond_pass,'lo',f"if unsigned lower [C==0] ({cond_pass}) "
        elif cond == 0b0100:
            cond_pass=1==self.N
            return cond_pass,'mi',f"if negative [N==1] ({cond_pass}) "
        elif cond == 0b0101:
            cond_pass=0==self.N
            return cond_pass,'pl',f"if positive or zero [N==0] ({cond_pass}) "
        elif cond == 0b0110:
            cond_pass=1==self.V
            return cond_pass,'vs',f"if overflow [V==1] ({cond_pass}) "
        elif cond == 0b0111:
            cond_pass=0==self.V
            return cond_pass,'vc',f"if not overflow [V==0] ({cond_pass}) "
        elif cond == 0b1000:
            cond_pass=(1==self.C) and (0==self.Z)
            return cond_pass,'hi',f"if unsigned higher [C==1 and Z==0] ({cond_pass}) "
        elif cond == 0b1001:
            cond_pass=(0==self.C) or (1==self.Z)
            return cond_pass,'ls',f"if unsigned lower or same [C==0 or Z==1] ({cond_pass}) "
        elif cond == 0b1010:
            cond_pass = (self.N==self.V)
            return cond_pass, 'ge', f"if signed greater or equal [N==V] ({cond_pass}) "
        elif cond == 0b1011:
            cond_pass = (self.N != self.V)
            return cond_pass, 'lt', f"if signed less than [N!=V] ({cond_pass}) "
        elif cond == 0b1100:
            cond_pass = self.Z==0 and (self.N != self.V)
            return cond_pass, 'gt', f"if signed greater than [Z==0 and N!=V] ({cond_pass}) "
        elif cond == 0b1101:
            cond_pass = self.Z==1 or (self.N != self.V)
            return cond_pass, 'le', f"if signed less than or equal [Z==1 or N!=V] ({cond_pass}) "
        elif cond==0b1110:
            cond_pass=True
            return cond_pass,'',""
        raise ValueError(f"Condition code {cond:04b} not decoded")


def format_hex(i:int)->str:
    return f'0x{(i&0xffff_ffff)>>16:04X}_{i&0xffff:04X}'


@dataclass
class SideEffect:
    gpr:dict[int,int]=field(default_factory=dict)  # Datapath side effects
    xpsr:dict[str,int]=field(default_factory=dict) # xPSR side effects
    mem:dict[int,int]=field(default_factory=dict)  # Memory side effects
    def writeback(self, d:datapath, mem:Memory)->list[str]:
        """
        Apply the given side effects to a datapath and memory

        :param d: datapath
        :param mem: memory
        :return: list of strings describing old and new values of each affected item
        """
        result=[]
        for n,val in self.gpr.items():
            result.append(f'r{n}: {format_hex(d[n])}->{format_hex(val)}')
            d[n]=val
        for x,psr in self.xpsr.items():
            if x[0].lower=='c':
                result.append(f'cpsr: {d.cpsr}->{psr}')
                d.cpsr.val=psr.val
            else:
                result.append(f'spsr: {d.spsr}->{psr}')
                d.spsr.val=psr.val
        for addr,val in self.mem.items():
            result.append(f'mem[{format_hex(addr)}]={format_hex(val)}')
            mem[addr]=val
        return result


class datapath:
    # register banks -- self._r[i_mode][rn] stores the register for
    # mode i_mode (which isn't the same as the mode numbers in table A2-1)
    # The processor has an idea of modes. Each mode might have a different privilege,
    # and different modes have some registers in common with other modes, while
    # some registers are *shadowed* and only visible in the given mode.
    #   A2-1 mode number    mode name
    # --------------------|-------------
    def __init__(self):
        self._gpr=[[0xc010_2ad0]*16 for i in range(7)] #general purpose register banks, constructed this way because [[0]*16]*7
                                             #makes a list of 7 references to the same list of 16 registers. Initialize them all
                                             #with a recognizable unlikely pattern so that we know when they are touched.
        # Shadowing is rather complicated:
        #   * usr and sys use all the same GPRs. Sys has its own spsr, while usr has no spsr.
        #   * irq, svc, abt, und each have their own shadowed r13 (stack pointer) and r14 (link register), and
        #       their own spsr
        #   * fiq has its own shadowed r8-r14 and its own spsr
        # This register shadowing is used without exception. Shadowed registers are never visible from other modes.
        # In order to access a shadowed register, the mode must be switched. All instructions always use the
        # appropriate shadowed registers. Instructions which switch modes have access to registers as described
        # in the instruction pseudocode.
        #
        # Therefore, we can set up an appropriate getter and setter for the registers, and have the instruction
        # functions use the getters and setters. We set up enough state space so that all of the registers can
        # be shadowed, and then leave it up to the getter and setter
        self.cpsr=xPSR() #reset goes into supervisor (svc) mode
        self[15]=0x0000_0000 #address of reset exception vector
        self._spsr=[xPSR(0xc010_2ad0) for i in range(7)]
    @property
    def spsr(self)->xPSR:
        return self._spsr[xPSR.mode_map[self.cpsr.M]]
    def __getitem__(self,rn:int)->int:
        return self._gpr[self.cpsr.gpr_bank(rn)][rn]
    def __setitem__(self,rn:int,val:int):
        self._gpr[self.cpsr.gpr_bank(rn)][rn]=0xFFFF_FFFF & val
    def r_prefetch(self,rn:int)->int:
        """
        Get the value of the given numbered register at the time that the instruction is executed.
        For all registers except r15 (PC), it's just the normal value. For PC, by the time the
        instruction has executed, the processor has already prefetched the next two instructions
        so PC points 8 past the current instruction address. The emulator doesn't do prefetch,
        so we simulate this artificially.

        :param rn: Number of register to get
        :return: Value of register considering prefetch, as stated above.
        """
        if rn==15:
            return self[15]+8
        else:
            return self[rn]
    def addr_mode_4(self, p:int, u:int, s:int, w:int, l:int, Rn:int, reglist:int):
        """
        Address mode 4 -- load and store multiple
        :param p:
        :param u:
        :param s:
        :param w:
        :param l:
        :param Rn:
        :param reglist:
        :return: tuple of (start_address, end_address, parsed_reglist, disasm_addr, disasm_writeback)
        :side effect: if requested by the address mode, writes address back to appropriate register
        """
        # PUSWL Rn RegList
        parsed_reglist=[i_reg for i_reg in range(16) if get_bits(reglist, i_reg, i_reg)]
        if p==0 and u==1:
            # A5.4.2 -- Load and store multiple - increment after
            start_address=self[Rn]
            end_address=start_address+(len(parsed_reglist)*4)-4
            if w==1:
                self[Rn]=end_address+4
            return (start_address,end_address,parsed_reglist,f"mem[r{Rn} 0x{start_address:08x}--0x{end_address:08x}]",f", write 0x{self[Rn]:08x} back to r{Rn} after" if w==1 else '')
        elif p==1 and u==1:
            # A5.4.3 -- Load and store multiple - increment before
            start_address=self[Rn]+4
            end_address=start_address+(len(parsed_reglist)*4)
            if w==1:
                self[Rn]=self[Rn]+(len(parsed_reglist)*4)
            return (start_address,end_address,parsed_reglist,f"mem[r{Rn} 0x{start_address:08x}--0x{end_address:08x}]",f", write 0x{self[Rn]:08x} back to r{Rn} before" if w==1 else '')
        elif p==0 and u==0:
            # A5.4.4 -- Load and store multiple - decrement after
            start_address=self[Rn]-(len(parsed_reglist)*4)+4
            end_address=self[Rn]
            if w==1:
                self[Rn]=self[Rn]-(len(parsed_reglist)*4)
            return (start_address,end_address,parsed_reglist,f"mem[ 0x{start_address:08x}--0x{end_address:08x} r{Rn}]",f", write 0x{self[Rn]:08x} back to r{Rn} after" if w==1 else '')
        elif p==1 and u==0:
            # A5.4.5 -- Load and store multiple - decrement before
            start_address=self[Rn]-(len(parsed_reglist)*4)
            end_address=self[Rn]-4
            if w==1:
                self[Rn]=self[Rn]-(len(parsed_reglist)*4)
            return (start_address,end_address,parsed_reglist,f"mem[ 0x{start_address:08x}--0x{end_address:08x} r{Rn}]",f", write 0x{self[Rn]:08x} back to r{Rn} before" if w==1 else '')
        raise NotImplementedError("addr_mode_4")
    def addr_mode_2(self, i:int, p:int, u:int, b:int, w:int, l:int, Rn:int, Rd:int, a:int, se:SideEffect)->tuple[int, str, str]:
        """
        A5.2 Addressing Mode 2 - Load and Store Word or Unsigned Byte

        There are nine formats used to calculate the address for a Load
        and Store Word or Unsigned Byte instruction. The general instruction
        syntax is: LDR|STR{<cond>}{B}{T} <Rd>, <addressing_mode>

        :param i: Immediate bit
        :param p: post-indexed addressing bit
        :param u: offset direction bit
        :param b: byte-sized access bit
        :param w: writeback bit
        :param l: load bit
        :param Rn: number of register containing base address
        :param Rd:
        :param a:
        :return:
        """
        if i==0 and p==1 and w==0:
            # A5.2.2 Load and store word or unsigned byte -- immediate offset
            if u==1:
                return 0xFFFF_FFFF & (self.r_prefetch(Rn)+a),f"[r{Rn},+{a}]",f"mem[r{Rn}+{a}]"
            else:
                return 0xFFFF_FFFF & (self.r_prefetch(Rn)-a),f"[r{Rn},-{a}]",f"mem[r{Rn}-{a}]"
        elif i==0 and p==0 and w==0:
            # A5.2.8 Load and store word or unsigned byte -- immediate post-indexed
            addr=self[Rn]
            if Rn==15:
                raise UNPREDICTABLE("Cannot use r15 in this context")
            if u==1:
                se.gpr[Rn]=self[Rn]+a
                return 0xFFFF_FFFF & (addr),f'[r{Rn}],#+{a}',f"mem[r{Rn}], r{Rn}+={a} (={self[Rn]-a:08x})"
            else:
                se.gpr[Rn]=self[Rn]-a
                return 0xFFFF_FFFF & (addr),f'[r{Rn}],#-{a}',f"mem[r{Rn}], r{Rn}-={a} (={self[Rn]+a:08x})"
        raise ValueError(f"Address not decoded - i={i}, p={p}, u={u}, b={b}, w={w}, l={l}, Rn={Rn}, a={a:03x}")
    def shifter_operand(self, i:int, shifter_operand:int)->tuple[int, int]:
        """
        Decode the shifter operand
        :param i: Immediate flag, bit 25 of instruction
        :param shifter_operand: Shifter operand, lower 12 bits of instruction
        :return: Tuple of shifter result, shifter carry out, disasm, desc
        """
        if i==1:
            # A5.1.3 Data-processing operands -- Immediate
            immed_8= get_bits(shifter_operand, 7, 0)
            rotate_imm= get_bits(shifter_operand, 11, 8)
            shifter_out= rotate_right(immed_8, rotate_imm * 2)
            if rotate_imm==0:
                shifter_carry_out=self.cpsr.C
            else:
                shifter_carry_out= get_bits(shifter_out, 31, 31)
            return shifter_out,shifter_carry_out,f'#{shifter_out}',f'{shifter_out}'
        else:
            if get_bits(shifter_operand, 4, 4)==0:
                # Immediate shifts
                if get_bits(shifter_operand, 6, 5)==0b00:
                    #A5.1.5 Data-processing operands -- Logical shift left by immediate
                    #(A5.1.4 is encoded by shift by zero)
                    m= get_bits(shifter_operand, 3, 0)
                    Rm=self.r_prefetch(m)
                    shift_imm= get_bits(shifter_operand, 11, 7)
                    if shift_imm==0:
                        #A5.1.4 Data-processing operands -- Register
                        return Rm,self.cpsr.C,f'r{m}',f'r{m}'
                    else:
                        shifter_out=Rm<<shift_imm
                        shifter_carry_out= get_bits(Rm, 32 - shift_imm, 32 - shift_imm)
                        return shifter_out,shifter_carry_out,f'r{m}, lsl #{shift_imm}',f'r{m} << {shift_imm}'
                elif get_bits(shifter_operand, 6, 5)==0b01:
                    #A5.1.7 Data-processing operands -- Logical shift right by immediate
                    m= get_bits(shifter_operand, 3, 0)
                    Rm=self.r_prefetch(m)
                    shift_imm= get_bits(shifter_operand, 11, 7)
                    if shift_imm==0:
                        #A5.1.4 Data-processing operands -- Register
                        shifter_operand=0
                        shifter_carry_out= get_bits(Rm, 31, 31)
                        return shifter_operand,shifter_carry_out,f'r{m}, lsr #32',f'r{m} >>> #32'
                    else:
                        shifter_out=(Rm & 0xffff_ffff)>>shift_imm
                        shifter_carry_out= get_bits(Rm, 32 - shift_imm, 32 - shift_imm)
                        return shifter_out,shifter_carry_out,f'r{m}, lsr #{shift_imm}',f'r{m} >>> {shift_imm}'
                elif get_bits(shifter_operand, 6, 5)==0b10:
                    #A5.1.9 Data-processing operands -- Arithmetic shift right by immediate
                    m= get_bits(shifter_operand, 3, 0)
                    Rm=self.r_prefetch(m)
                    shift_imm= get_bits(shifter_operand, 11, 7)
                    if shift_imm==0:
                        shifter_carry_out= get_bits(Rm, 31, 31)
                        if shifter_carry_out==0:
                            shifter_operand=0
                        else:
                            shifer_operand=0xffff_ffff
                        return shifter_operand,shifter_carry_out,f'r{m}, asr #32',f'r{m} >> 32'
                    else:
                        shifter_out= arithmetic_shift_right(Rm & 0xffff_ffff, shift_imm)
                        shifter_carry_out= get_bits(Rm, 32 - shift_imm, 32 - shift_imm)
                        return shifter_out,shifter_carry_out,f'r{m}, asr #{shift_imm}',f'r{m} >> {shift_imm}'
            elif get_bits(shifter_operand, 7, 7)==0:
                # Register shifts
                if get_bits(shifter_operand, 6, 5)==0b00:
                    #A5.1.6 - Data-processing operands -- logical shift left by register
                    raise NotImplementedError("A5.1.6")
                elif get_bits(shifter_operand, 6, 5)==0b01:
                    #A5.1.7 - Data-processing operands -- logical shift right by immediate
                    raise NotImplementedError("A5.1.7")
                    Rm= get_bits(shifter_operand, 3, 0)
                    shift_imm= get_bits(shifter_operand, 11, 7)
        raise ValueError(f"Shifter operand not decoded, i={i}, shifter_operand=0x{shifter_operand:03x}")
    def in_priv_mode(self):
        return self.cpsr.M!=xPSR.usr
    def has_spsr(self):
        return self.M not in (self.usr,self.sys)
    def __str__(self):
        result=f"cpsr={self.cpsr}\n"
        result+=("        "+"   |    ".join([('>' if x==self.cpsr.M else ' ')+xPSR.mode_name_map[x]+('<' if x==self.cpsr.M else ' ') for x in xPSR.display_order]))
        for ri in range(0,8):
            result+=f'\n r{ri:02d} '+format_hex(self._gpr[xPSR.mode_map[xPSR.usr]][ri])
        for ri in range(8,13):
            result+=f'\n r{ri:02d} '+format_hex(self._gpr[xPSR.mode_map[xPSR.usr]][ri])+' '*68+format_hex(self._gpr[xPSR.mode_map[xPSR.fiq]][ri])
        for ri in range(13,15):
            result+=f'\n r{ri:02d} '+format_hex(self._gpr[xPSR.mode_map[xPSR.usr]][ri])+'                '+" | ".join([format_hex(self._gpr[xPSR.mode_map[x]][ri]) for x in xPSR.display_order[2:]])
        for ri in range(15,16):
            result+=f'\n r{ri:02d} '+format_hex(self._gpr[xPSR.mode_map[xPSR.usr]][ri])
        result+="\nspsr                           "+" | ".join([format_hex(self._spsr[xPSR.mode_map[x]].val) for x in xPSR.display_order[2:]])
        result+="\n                                "+" |  ".join([xPSR.str_psr(self._spsr[xPSR.mode_map[x]].val) for x in xPSR.display_order[2:]])
        return result


CP15_reg1_UBit=1

# All instruction functions have the following form:
#     def INS(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory)->tuple[str,str,SideEffect]:
# where:
#  * INS is the mnemonic for the instruction
#  * fields is a dictionary of parsed field values, as returned by parse_bitpat()
#  * d is an ARMv4T-compatible datapath class, meaning it has ARM flags, xPSR, etc.
#  * mem is a Memory object
# The return value is a tuple of:
#  * Disassembly in ARM assembly language
#  * English/math description of what happened, including what values are read, calculated, and
#    written.
#  * SideEffect object. This will be applied to the datapath and memory after this function
#    runs, before the next function runs.
# It would be cool if we could have separate functions for disassembly and execution, but we will
# interleave these, to use the same pseudocode structure for both disassembly and execution.
# This doesn't seem efficient, but we don't care much anyway, since we are running in Python
# and not using particularly efficient code. We optimize for readability, non-duplication,
# regularity, and matching the form of the ARM manual pseudocode.


def LDR(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory,se:SideEffect)->tuple[str,str]:
    """
    A4.1.23 -- LDR

    LDR (Load Register) loads a word from a memory address.

    If the PC is specified as register <Rd>, the instruction loads a data word which it
    treats as an address, then branches to that address. In ARMv5T and above, bit[0] of
    the loaded value determines whether execution continues after this branch in ARM state
    or in Thumb state, as though a BX (loaded_value) instruction had been executed. In
    earlier versions of the architecture, bits[1:0] of the loaded value are ignored and
    execution continues in ARM state, as though a MOV PC,(loaded_value) instruction had
    been executed.
    """
    cond_pass,cond_disasm,cond_desc=d.cpsr.ConditionPassed(fields['c'])
    if cond_pass:
        addr,addr_disasm,addr_desc=d.addr_mode_2(i=fields['i'], p=fields['p'], u=fields['u'], b=0, w=fields['w'], l=1,
                                                 Rn=fields['n'], Rd=fields['d'], a=fields['a'], se=se)
        data=mem[addr,4]
        if fields['d']==15:
            se.gpr[15]=data & 0xFFFFFFFC
        else:
            se.gpr[fields['d']]=data
    disasm="ldr"+cond_disasm+f" r{fields['d']}, {addr_disasm}"
    desc=cond_desc+f"r{fields['d']}={addr_desc} (mem[0x{addr:08x}]=0x{data:08x})"
    return disasm,desc


def MSR(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory,se:SideEffect):
    """
    A4.1.39 -- MSR
    :param fields:
    :param d:
    :param mem:
    :return:
    """
    UnallocMask=0x0FFF_FF00
    UserMask=0xF000_0000
    PrivMask=0x0000_00DF # Note -- documentation is wrong, doesn't include I and F bits or highest M bit
    StateMask=0x0000_0020
    if fields['i']==1:
        operand= rotate_right(fields['j'], fields['k'] * 2)
    else:
        Rm=get_bits(fields['k'],3,0)
        operand=d[Rm]
    field_mask=[fields['f'] & (1<<x) for x in range(4)]
    byte_mask=((0x0000_00FF if field_mask[0] else 0x0000_0000) |
               (0x0000_FF00 if field_mask[1] else 0x0000_0000) |
               (0x00FF_0000 if field_mask[2] else 0x0000_0000) |
               (0xFF00_0000 if field_mask[3] else 0x0000_0000))
    byte_mask_decode=(('c' if field_mask[0] else '')+
                      ('x' if field_mask[1] else '')+
                      ('s' if field_mask[2] else '')+
                      ('f' if field_mask[3] else ''))
    cond_pass,cond_disasm,cond_desc=d.cpsr.ConditionPassed(fields['c'])
    if cond_pass:
        if operand & UnallocMask:
            raise UNPREDICTABLE("Trying to set bits that aren't allocated")
        if fields['r']==0:
            if d.in_priv_mode():
                if operand & StateMask:
                    raise UNPREDICTABLE("Trying to change THUMB mode the wrong way")
                else:
                    mask=byte_mask & (UserMask|PrivMask)
            else:
                mask=byte_mask & UserMask
            cpsr=xPSR()
            cpsr.val=(d.cpsr.val & bit_not(mask)) | (operand & mask)
            se.xpsr['c']=cpsr
        else:
            if d.has_spsr():
                mask=byte_mask & (UserMask | PrivMask | StateMask)
                spsr=xPSR()
                spsr.val=(d.spsr.val & bit_not(mask)) | (operand & mask)
                se.xpsr['s']=spsr
            else:
                raise UNPREDICTABLE("No SPSR in current mode")

    return f"msr{cond_disasm} {'c' if fields['r']==0 else 's'}psr_{byte_mask_decode}, "+(f"#{operand}" if fields['i'] else f"r{Rm}"),None


def MOV(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory,se:SideEffect):
    """
    A4.1.35 -- MOV
    :param fields:
    :param d:
    :param mem:
    :return:
    """
    out, c, shifter_operand_disasm,shifter_operand_desc = d.shifter_operand(fields['i'], fields['j'])
    cond_pass,cond_disasm,cond_desc=d.cpsr.ConditionPassed(fields['c'])
    if cond_pass:
        se.gpr[fields['d']]=out
        if fields['s']==1 and fields['d']==15:
            se.xpsr['c']=xPSR()
            se.xpsr['c'].val=d.SPSR.val
        elif fields['s']==1:
            se.xpsr['c']=xPSR()
            se.xpsr['c'].N= get_bits(d[fields['d']], 31, 31)
            se.xpsr['c'].Z=1 if d[fields['d']]==0 else 0
            se.xpsr['c'].C=c
          # se.xpsr['c'].V=unaffected
    disasm=f"mov{cond_disasm}{'s' if fields['s'] else ''} r{fields['d']}, {shifter_operand_disasm}"
    return disasm,f"{cond_desc}r{fields['d']}={shifter_operand_desc}"


def SUB(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory,se:SideEffect):
    """
    A4.1.106 -- SUB
    :param fields:
    :param d:
    :param mem:
    :return:
    """
    cond_pass, cond_disasm, cond_desc = d.cpsr.ConditionPassed(fields['c'])
    if cond_pass:
        shifter_operand,shifter_carry_out,shifter_operand_disasm,shifter_operand_desc=d.shifter_operand(fields['i'], fields['j'])
        subA=d[fields['n']]
        subB=shifter_operand
        se.gpr[fields['d']]=subA-subB
        if fields['s']==1 and fields['d']==15:
            se.xpsr['c']=xPSR()
            se.xpsr['c'].val=d.SPSR.val
        elif fields['s']==1:
            se.xpsr['c']=xPSR()
            se.xpsr['c'].N= get_bits(d[fields['d']], 31, 31)
            se.xpsr['c'].Z=1 if d[fields['d']]==0 else 0
            se.xpsr['c'].C=~borrow_from(subA,subB)
            se.xpsr['c'].V=overflow_from_sub(subA,subB)
    disasm='sub'+cond_disasm+('s' if fields['s'] else '')+f' r{fields["d"]}, r{fields["n"]}, {shifter_operand_disasm}'
    desc=cond_desc+f"r{fields['d']}=r{fields['n']}-{shifter_operand_disasm}"
    return disasm,desc


def B(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory,se:SideEffect):
    """
    A4.1.5 -- B, BL
    :param fields:
    :param d:
    :param mem:
    :return:
    """
    cond_pass, cond_disasm, cond_desc = d.cpsr.ConditionPassed(fields['c'])
    signed_immed_24 = fields['k']
    delta = sign_extend(signed_immed_24, 24, 30) << 2
    new_pc = (d.r_prefetch(15) + delta) & 0xFFFF_FFFF  # Pipelined PC (base from which to use delta) is needed
    if cond_pass:
        if fields['l']==1:
            se.gpr[14]=d[15]+4 #non-prefetch value
        se.gpr[15]=new_pc
    disasm='b'+('l' if fields['l'] else '')+cond_disasm+' '+format_hex(new_pc)
    return disasm,None


def CMP(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory,se:SideEffect):
    """
    A4.1.15 -- CMP
    :param fields:
    :param d:
    :param mem:
    :return:
    """
    cond_pass, cond_disasm, cond_desc = d.cpsr.ConditionPassed(fields['c'])
    if cond_pass:
        shifter_operand,shifter_carry_out,shifter_operand_disasm,shifter_operand_desc=d.shifter_operand(fields['i'], fields['j'])
        subA=d[fields['n']]
        subB=shifter_operand
        alu_out=subA-subB
        se.xpsr['c'] = xPSR()
        se.xpsr['c'].N= get_bits(alu_out, 31, 31)
        se.xpsr['c'].Z=1 if alu_out==0 else 0
        se.xpsr['c'].C=~borrow_from(subA,subB)
        se.xpsr['c'].V=overflow_from_sub(subA,subB)
    disasm=f"cmp{cond_disasm} r{fields['n']}, {shifter_operand_disasm}"
    desc=cond_desc+f"N,Z,C,V=r{fields['n']}(0x{subA:08x})<=>{shifter_operand_desc}(0x{subB:08x})"
    return disasm, desc


def STR(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory):
    """
    A4.1.99 -- STR
    :param fields:
    :param d:
    :param mem:
    :return:
    """
    cond_pass,cond_decode=d.ConditionPassed(fields['c'])
    if cond_pass:
        addr,addr_decode=d.addr_mode_2(i=fields['i'], p=fields['p'], u=fields['u'], b=0, w=fields['w'], l=1, Rn=fields['n'], Rd=fields['d'], a=fields['a'])
        data=d[fields['d']]+(4 if fields['d']==15 else 0)
        mem[addr, 4]=data
    return cond_decode+f"{addr_decode}=r{fields['d']} (=0x{data:08x})"


def ADD(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory):
    """
    A4.1.3 -- ADD
    :param fields:
    :param d:
    :param mem:
    :return:
    """
    cond_pass, cond_decode = d.ConditionPassed(fields['c'])
    shifter_operand, _, decode_disasm = d.shifter_operand(fields['i'], fields['j'])
    if cond_pass:
        subA=d[fields['n']]
        subB=shifter_operand
        d[fields['d']]=subA+subB
        if fields['s']==1 and fields['d']==15:
            d.CPSR=d.SPSR
        elif fields['s']==1:
            d.N= get_bits(d[fields['d']], 31, 31)
            d.Z=1 if d[fields['d']]==0 else 0
            d.C=~borrow_from(subA,subB)
            d.V=overflow_from_add(subA,subB)
    return cond_decode+f"r{fields['d']}=r{fields['n']}+{decode_disasm}"


def BIC(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory,se:SideEffect):
    """
    A4.1.6 -- BIC
    :param fields:
    :param d:
    :param mem:
    :return:
    """
    cond_pass, cond_disasm, cond_desc = d.cpsr.ConditionPassed(fields['c'])
    if cond_pass:
        shifter_operand,shifter_carry_out,shifter_operand_disasm,shifter_operand_desc=d.shifter_operand(fields['i'], fields['j'])
        subA=d.r_prefetch(fields['n'])
        subB=shifter_operand
        se.gpr[fields['d']]=d[fields['n']] & ~(shifter_operand & 0xffff_ffff)
        if fields['s']==1 and fields['d']==15:
            se.xpsr['c']=xPSR()
            se.xpsr['c'].val=d.SPSR.val
        elif fields['s']==1:
            se.xpsr['c']=xPSR()
            se.xpsr['c'].N= get_bits(d[fields['d']], 31, 31)
            se.xpsr['c'].Z=1 if d[fields['d']]==0 else 0
            se.xpsr['c'].C=shifter_carry_out
           #se.xpsr['c'].V=unaffected
    disasm='bic'+cond_disasm+('s' if fields['s'] else '')+f" r{fields['d']}, r{fields['n']}, {shifter_operand_disasm}"
    desc=cond_desc+f"r{fields['d']}=r{fields['n']}&~({shifter_operand_desc})"
    return disasm,desc


def BX(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory):
    """
    A4.1.10 -- BX
    :param fields:
    :param d:
    :param mem:
    :return:
    """
    cond_pass, cond_decode = d.ConditionPassed(fields['c'])
    if cond_pass:
        d[15]=d[fields['m']] & 0xFFFF_FFFC
    return cond_decode+f"r15=r{fields['m']}=0x{d[fields['m']]:08x}"


def TST(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory):
    """
    A4.1.117 -- TST
    :param fields:
    :param d:
    :param mem:
    :return:
    """
    cond_pass, cond_decode = d.ConditionPassed(fields['c'])
    if cond_pass:
        shifter_operand,shifter_carry_out,decode_disasm=d.shifter_operand(fields['i'], fields['j'])
        alu_out=d[fields['n']] & shifter_operand
        d.N= get_bits(alu_out, 31, 31)
        d.Z=1 if alu_out==0 else 0
        d.C=shifter_carry_out
        #d.V=unaffected
    return cond_decode+f"N,Z,C,V=r{fields['n']}&({decode_disasm})"


def STM_1(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory):
    """
    A4.1.97 -- STM (1)
    :param fields:
    :param d:
    :param mem:
    :return:
    """
    cond_pass,cond_decode=d.ConditionPassed(fields['c'])
    if cond_pass:
        start_address,end_address,parsed_reglist,disasm_addr,disasm_writeback=d.addr_mode_4(p=fields['p'], u=fields['u'], s=0, w=fields['w'], l=0, Rn=fields['n'], reglist=fields['r'])
        address=start_address
        for reg in parsed_reglist:
            mem[address,4]=d[reg]
            address+=4
    return cond_decode+f"{disasm_addr}={list(parsed_reglist)}{disasm_writeback}"


def AND(fields:dict[str,tuple[int,int]],d:datapath,mem:Memory):
    """
    A4.1.4 -- AND
    :param fields:
    :param d:
    :param mem:
    :return:
    """
    cond_pass, cond_decode = d.ConditionPassed(fields['c'])
    shifter_operand, shifter_carry_out, decode_disasm = d.shifter_operand(fields['i'], fields['j'])
    if cond_pass:
        subA=d[fields['n']]
        subB=shifter_operand
        d[fields['d']]=subA & subB
        if fields['s']==1 and fields['d']==15:
            d.CPSR=d.SPSR
        elif fields['s']==1:
            d.N= get_bits(d[fields['d']], 31, 31)
            d.Z=1 if d[fields['d']]==0 else 0
            d.C=shifter_carry_out
           #d.V=unaffected
    return cond_decode+f"r{fields['d']}=r{fields['n']}&{decode_disasm}"


class ARM(datapath):
    # Common fields
    # 1    -- instruction words for this instruction must have a 1 in
    #         this spot, or else it's not this instruction.
    # 0    -- instruction words for this instruction must have a 0 in
    #         this spot, or else it's not this instruction.
    # zzzz -- should be zero. These aren't documented as zero because
    #         presumably in the future, an extension could be added
    #         to this instruction using these bits.
    # oooo -- should be one. Same as zzzz except should be 1.
    # cccc -- comdition code
    # i - (bit 25) immediate/~register flag
    # p - (bit 24) pre/~post-indexed flag
    # u - (bit 23) add/~subtract offset to base
    # b - (bit 22) byte/~word flag
    # s - (bit 22) for ldm that includes r15 (pc), indicates if restore is from SPSR
    # w - (bit 21) writeback flag, also usermode access if p==0
    # l - (bit 20) load/~store flag for memory access
    # s - (bit 20) set cpsr flags. If set, NZCV flags in CPSR will
    #              be updated with values calculated by this instruction.
    # jjjj -- shifter operand, see A5.1. Interpreted along with i
    #         to get a shifted immediate or shifted register.
    # kkkk -- 24-bit signed immediate word count
    #         4-bit rotate amount for msr
    # aaaa -- encoded address, see A5.2.
    # nnnn -- first source register address
    # mmmm -- second source register address
    # dddd -- destionation register address
    # rrrr -- register list for load or store multiple
    decode_encoded = {
        'cccc01ipu0w1nnnnddddaaaaaaaaaaaa': LDR,     #A4.1.23
        'cccc00i10r10ffffooookkkkjjjjjjjj': MSR,     #A4.1.39
        'cccc00i1101szzzzddddjjjjjjjjjjjj': MOV,     #A4.1.35
        'cccc00i0010snnnnddddjjjjjjjjjjjj': SUB,     #A4.1.106
        'cccc00i10101nnnnzzzzjjjjjjjjjjjj': CMP,
        'cccc101lkkkkkkkkkkkkkkkkkkkkkkkk': B,
        'cccc01ipu0w0nnnnddddaaaaaaaaaaaa': STR,     #A4.1.99
        'cccc00i0100snnnnddddjjjjjjjjjjjj': ADD,     #A4.1.3
        'cccc00i1110snnnnddddjjjjjjjjjjjj': BIC,
        'cccc00010010oooooooooooo0001mmmm': BX,
        'cccc00i10001nnnnzzzzjjjjjjjjjjjj': TST,
        'cccc100pu0w0nnnnrrrrrrrrrrrrrrrr': STM_1,   #A4.1.97
        'cccc00i0000snnnnddddjjjjjjjjjjjj': AND,     #A4.1.4
    }
    def __init__(self):
        super().__init__()
        self.decode_dict = {}
        for pat, opcode in self.decode_encoded.items():
            need0,need1,fields= parse_bitpat(pat)
            self.decode_dict[(need0, need1)] = (opcode,fields)
    def decode(self,ins:int):
        for (need0,need1),(opcode,fields) in self.decode_dict.items():
            if bits_match(ins, need0, need1):
                field_vals = decode_bitpat(ins,fields)
                return (opcode,field_vals)
        raise ValueError(f"Couldn't decode instruction {ins:08x}")
    def exec(self,mem:Memory):
        pc=self[15]
        ins=mem[pc,4]
        print(f"{format_hex(pc)} - {format_hex(ins)} ",end='')
        opcode,field_vals=self.decode(ins)
        se=SideEffect()
        se.gpr[15]=pc+4
        disasm,desc=opcode(field_vals,self,mem,se=se)
        se.writeback(d=self, mem=mem)
        return disasm,desc,se


if __name__=="__main__":
    mem=AddressSpace([(0x0000_0000,ROM('UartTest.bin',0x4000)),
                      (0x4000_0000,RAM(0x1000))])
    print(f'{mem[0x0000_0000]:08x}')
    print(f'{mem[0x4000_0000]:08x}')
    proc=ARM()
    print(proc)
    while True:
        disasm,desc,se=proc.exec(mem)
        if disasm is None:
            raise ValueError("Didn't disassemble")
        print(f'{disasm:20s}{desc},{se}')
#        print(proc)


