pub mod elf {
    use std::{collections::HashMap, fmt::Debug, fs, hash::Hash, ops::Add as NumAdd};
    #[derive(PartialEq, Clone, Copy, Debug)]
    enum Bitness {
        _32,
        _64,
    }

    trait IntoOperhand {
        fn build(&self) -> Box<dyn Operhand>;
    }

    impl<X: Operhand + 'static + Clone> IntoOperhand for X {
        fn build(&self) -> Box<dyn Operhand> {
            Box::new(self.clone())
        }
    }

    impl Into<Adress> for u64 {
        fn into(self) -> Adress {
            Adress { data: self }
        }
    }

    #[derive(Clone, Copy)]
    struct Adress {
        data: u64,
    }

    impl Adress {
        fn to_bytes(&self) -> Vec<u8> {
            self.data.to_le_bytes().to_vec()
        }
    }

    impl NumAdd<u64> for Adress {
        type Output = Adress;
        fn add(self, rhs: u64) -> Self::Output {
            Adress {
                data: self.data + rhs,
            }
        }
    }

    impl Into<Register> for &str {
        fn into(self) -> Register {
            let mut iter = self.clone().chars();
            let mut first_char = iter.next().unwrap();
            let mut regcode = self.to_string();
            let mut bits;
            let mut mode = 0b11;
            let mut ofsset = None;

            match first_char.to_ascii_lowercase() {
                // [ebp-2]
                '[' => {
                    first_char = iter.next().unwrap();
                    regcode = first_char.to_string();
                    regcode.push(iter.next().unwrap());
                    regcode.push(iter.next().unwrap());

                    let last_char = iter.next_back().unwrap();
                    if last_char != ']' {
                        panic!();
                    }
                    let number: String = iter.collect();
                    if number.len() > 0 {
                        mode = 0b10;
                        ofsset = Some(number.parse().unwrap());
                    } else {
                        mode = 0b00;
                    }
                }
                _ => {}
            }

            match first_char.to_ascii_lowercase() {
                'r' => bits = Bitness::_64,
                'e' => bits = Bitness::_32,
                _ => panic!("expected r or e"),
            };
            Register {
                bits,
                data: regcode.as_str().into(),
                ofsset,
                mode,
            }
        }
    }

    impl IntoOperhand for String {
        fn build(&self) -> Box<dyn Operhand> {
            let reg: &str = self.as_str();
            let reg: Register = reg.into();
            Box::new(reg)
        }
    }

    impl IntoOperhand for &str {
        fn build(&self) -> Box<dyn Operhand> {
            let reg: &str = *self;
            let reg: Register = reg.into();
            Box::new(reg)
        }
    }

    impl Into<Registers> for &str {
        fn into(self) -> Registers {
            let last_two_chars = self.as_bytes();
            let last_two_chars: String = [last_two_chars[1] as char, last_two_chars[2] as char]
                .iter()
                .collect();

            match last_two_chars.to_ascii_lowercase().as_str() {
                "ax" => Registers::Ax,
                "cx" => Registers::Cx,
                "bx" => Registers::Bx,
                "dx" => Registers::Dx,
                "sp" => Registers::Sp,
                "bp" => Registers::Bp,
                "si" => Registers::Si,
                "di" => Registers::Di,
                "08" => Registers::_8,
                "09" => Registers::_9,
                "10" => Registers::_10,
                "11" => Registers::_11,
                "12" => Registers::_12,
                "13" => Registers::_13,
                "14" => Registers::_14,
                "15" => Registers::_15,

                _ => panic!("{} is not a register", self),
            }
        }
    }

    #[derive(Clone, Copy, Debug)]
    enum Registers {
        Ax,
        Cx,
        Dx,
        Bx,
        Sp,
        Bp,
        Si,
        Di,
        _8,
        _9,
        _10,
        _11,
        _12,
        _13,
        _14,
        _15,
    }

    #[derive(Clone, Copy, Debug)]
    struct Register {
        data: Registers,
        bits: Bitness,
        ofsset: Option<i32>,
        mode: u8,
    }
    impl Register {
        fn mode(&self) -> u8 {
            self.mode
        }

        fn offest(&self) -> Option<i32> {
            self.ofsset
        }

        fn index(&self) -> u8 {
            match self.data {
                Registers::Ax => 0b0000,
                Registers::Cx => 0b0001,
                Registers::Dx => 0b0010,
                Registers::Bx => 0b0011,
                Registers::Sp => 0b0100,
                Registers::Bp => 0b0101,
                Registers::Si => 0b0110,
                Registers::Di => 0b0111,
                Registers::_8 => 0b1000,
                Registers::_9 => 0b1001,
                Registers::_10 => 0b1010,
                Registers::_11 => 0b1011,
                Registers::_12 => 0b1100,
                Registers::_13 => 0b1101,
                Registers::_14 => 0b1110,
                Registers::_15 => 0b1111,
            }
        }
    }

    #[derive(Clone, Debug)]
    struct Immediate {
        data: u64,
        size: Bitness,
    }

    impl Immediate {
        fn to_bytes(&self) -> Vec<u8> {
            let bytes;
            if self.size == Bitness::_64 {
                bytes = self.data.to_le_bytes().to_vec();
            } else {
                bytes = (self.data as u32).to_le_bytes().to_vec();
            }
            bytes
        }

        fn to_bytes_sized(&self, size: Bitness) -> Vec<u8> {
            let bytes;
            if size == Bitness::_64 {
                bytes = self.data.to_le_bytes().to_vec();
            } else {
                bytes = (self.data as u32).to_le_bytes().to_vec();
            }
            bytes
        }
    }

    impl IntoOperhand for u64 {
        fn build(&self) -> Box<dyn Operhand> {
            let size = Bitness::_64;
            // if *self < i32::MAX as u64 {
            //     size = Bitness::_32;
            // }
            Box::new(Immediate { data: *self, size })
        }
    }

    //01100111

    trait Instruction {
        fn to_bytes(&self, asm: &Assembly) -> Vec<ByteOrLabel>;
    }

    impl Instruction for u8 {
        fn to_bytes(&self, asm: &Assembly) -> Vec<ByteOrLabel> {
            vec![ByteOrLabel::Byte(*self)]
        }
    }

    impl Instruction for Vec<u8> {
        fn to_bytes(&self, _: &Assembly) -> Vec<ByteOrLabel> {
            let mut res = vec![];
            for byte in self {
                res.push(ByteOrLabel::Byte(*byte));
            }
            res
        }
    }

    impl Instruction for Vec<ByteOrLabel> {
        fn to_bytes(&self, _: &Assembly) -> Vec<ByteOrLabel> {
            self.clone()
        }
    }

    #[derive(Clone, Debug)]
    struct EncodeArgs<'a> {
        opcode: Vec<u8>,
        digit: Option<u8>,
        isr: bool,
        plusreg: bool,
        operhandsize: Option<Bitness>,
        left: Option<&'a dyn Operhand>,
        right: Option<&'a dyn Operhand>,
    }

    impl Default for EncodeArgs<'_> {
        fn default() -> Self {
            EncodeArgs {
                opcode: vec![0x90],
                digit: None,
                isr: false,
                plusreg: false,
                operhandsize: None,
                left: None,
                right: None,
            }
        }
    }

    struct Mov {
        left: Register,
        right: Box<dyn Operhand>,
    }

    impl Instruction for Mov {
        fn to_bytes(&self, asm: &Assembly) -> Vec<ByteOrLabel> {
            let imm = self.right.immediate().is_some();
            let mut left = &self.left.clone();
            let mut right = self.right.as_ref();
            let label = self.right.label().is_some();
            let rmisleft = self.left.mode() != 0b11;

            let opcode: u8 = if imm || label {
                0xB8
            } else {
                if rmisleft {
                    left = right.register().unwrap();
                    right = &self.left;
                    0x89
                } else {
                    0x8B
                }
            };
            let operhandsize = if imm {
                Some(self.right.immediate().unwrap().size)
            } else if label {
                Some(Bitness::_64)
            } else {
                None
            };
            let bytes = Assembly::encode_instructon(EncodeArgs {
                opcode: vec![opcode],
                isr: !(imm || label),
                plusreg: imm || label,
                operhandsize,
                left: Some(left),
                right: Some(right),
                ..Default::default()
            });
            bytes
        }
    }

    struct Call {
        right: Box<dyn Operhand>,
    }

    impl Instruction for Call {
        fn to_bytes(&self, asm: &Assembly) -> Vec<ByteOrLabel> {
            let mut opcode = ByteOrLabel::Byte(0xFF);
            let mut operhands = vec![];
            if let Some(label) = self.right.label() {
                let new_label = Label {
                    name: label.name.clone(),
                    offset: true,
                    size: 4,
                };
                opcode = ByteOrLabel::Byte(0xE8);
                operhands.push(ByteOrLabel::Label(new_label));
            } else if let Some(register) = self.right.register() {
                if register.bits != Bitness::_64 {
                    panic!("Expected a 64 bit adress for call");
                }
                let modrm = (0b11 << 6) + (2 << 3) + register.index();
                operhands.push(ByteOrLabel::Byte(modrm));
            }
            let mut res = vec![opcode];
            res.extend(operhands);
            res
        }
    }

    struct Jump {
        left: Box<dyn Operhand>,
    }

    impl Instruction for Jump {
        fn to_bytes(&self, asm: &Assembly) -> Vec<ByteOrLabel> {
            let res = Assembly::encode_instructon(EncodeArgs {
                opcode: vec![0xE9],
                operhandsize: Some(Bitness::_32),
                left: Some(self.left.as_ref()),
                ..Default::default()
            });

            res
        }
    }

    struct Jnz {
        left: Box<dyn Operhand>,
    }

    impl Instruction for Jnz {
        fn to_bytes(&self, asm: &Assembly) -> Vec<ByteOrLabel> {
            let res = Assembly::encode_instructon(EncodeArgs {
                opcode: vec![0x0F, 0x85],
                operhandsize: Some(Bitness::_32),
                left: Some(self.left.as_ref()),
                ..Default::default()
            });

            res
        }
    }

    struct Jz {
        left: Box<dyn Operhand>,
    }

    impl Instruction for Jz {
        fn to_bytes(&self, asm: &Assembly) -> Vec<ByteOrLabel> {
            let res = Assembly::encode_instructon(EncodeArgs {
                opcode: vec![0x0F, 0x84],
                operhandsize: Some(Bitness::_32),
                left: Some(self.left.as_ref()),
                ..Default::default()
            });

            res
        }
    }

    struct Add {
        left: Register,
        right: Box<dyn Operhand>,
    }

    impl Instruction for Add {
        fn to_bytes(&self, asm: &Assembly) -> Vec<ByteOrLabel> {
            let imm = self.right.immediate().is_some();

            let rmisleft = self.left.mode() != 0b11;
            let mut left = &self.left.clone();
            let mut right = self.right.as_ref();
            let opcode: Vec<u8> = if imm {
                vec![0x81]
            } else {
                if rmisleft {
                    left = right.register().unwrap();
                    right = &self.left;
                    vec![0x01]
                } else {
                    vec![0x03]
                }
            };

            //let opcode = if imm { vec![0x81] } else { vec![0x03] };
            let digit = if imm { Some(0) } else { None };
            let operhandsize = if imm { Some(Bitness::_32) } else { None };
            let bytes = Assembly::encode_instructon(EncodeArgs {
                opcode,
                digit,
                operhandsize,
                isr: !imm,
                left: Some(left),
                right: Some(right),
                ..Default::default()
            });
            bytes
        }
    }

    struct Sub {
        left: Register,
        right: Box<dyn Operhand>,
    }

    impl Instruction for Sub {
        fn to_bytes(&self, asm: &Assembly) -> Vec<ByteOrLabel> {
            let imm = self.right.immediate().is_some();

            let opcode = if imm { vec![0x81] } else { vec![0x2B] };
            let digit = if imm { Some(5) } else { None };
            let operhandsize = if imm { Some(Bitness::_32) } else { None };
            let bytes = Assembly::encode_instructon(EncodeArgs {
                opcode,
                digit,
                operhandsize,
                isr: !imm,
                left: Some(&self.left),
                right: Some(self.right.as_ref()),
                ..Default::default()
            });
            bytes
        }
    }

    struct Cmp {
        left: Register,
        right: Box<dyn Operhand>,
    }

    impl Instruction for Cmp {
        fn to_bytes(&self, asm: &Assembly) -> Vec<ByteOrLabel> {
            let imm = self.right.immediate().is_some();
            let opcode = if imm { vec![0x81] } else { vec![0x39] };
            let digit = if imm { Some(7) } else { None };
            let operhandsize = if imm {
                Some(self.right.immediate().unwrap().size)
            } else {
                None
            };
            let bytes = Assembly::encode_instructon(EncodeArgs {
                opcode,
                digit,
                operhandsize,
                isr: !imm,
                left: Some(&self.left),
                right: Some(self.right.as_ref()),
                ..Default::default()
            });
            bytes
        }
    }
    struct Mult {
        left: Register,
        right: Register,
    }
    impl Instruction for Mult {
        fn to_bytes(&self, asm: &Assembly) -> Vec<ByteOrLabel> {
            let opcode = vec![0x0f, 0xAF];
            let bytes = Assembly::encode_instructon(EncodeArgs {
                opcode,
                isr: true,
                left: Some(&self.left),
                right: Some(&self.right),
                ..Default::default()
            });
            bytes
        }
    }

    trait Operhand: Debug {
        fn rex_64(&self) -> bool {
            return false;
        }

        fn register(&self) -> Option<&Register> {
            None
        }

        fn immediate(&self) -> Option<&Immediate> {
            None
        }

        fn label(&self) -> Option<&Label> {
            None
        }
    }

    impl Operhand for Register {
        fn rex_64(&self) -> bool {
            return self.bits == Bitness::_64;
        }
        fn register(&self) -> Option<&Register> {
            Some(self)
        }
    }

    impl<T: Operhand> Operhand for &T {}

    impl Operhand for Immediate {
        fn immediate(&self) -> Option<&Immediate> {
            Some(self)
        }
    }

    #[derive(Clone, Debug)]
    struct Label {
        name: String,
        offset: bool,
        size: u8,
    }
    impl Label {
        fn new<T: Into<String>>(name: T) -> Label {
            Label {
                name: name.into(),
                offset: false,
                size: 8,
            }
        }
    }
    impl Hash for Label {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.name.hash(state);
        }
    }
    impl PartialEq for Label {
        fn eq(&self, other: &Self) -> bool {
            self.name == other.name
        }
    }
    impl Eq for Label {}
    impl Operhand for Label {
        fn label(&self) -> Option<&Label> {
            Some(self)
        }
    }
    #[derive(Debug, Clone)]
    enum ByteOrLabel {
        Byte(u8),
        Label(Label),
    }

    struct Assembly {
        entry: Adress,
        instruction_labels: HashMap<Label, usize>,
        data_labels: HashMap<Label, usize>,
        data: Vec<u8>,
        instructions: Vec<Box<dyn Instruction>>,
    }

    const SYSCALL_ARGS: [&str; 7] = ["rax", "rdi", "rsi", "rdx", "r10", "r8", "r9"];

    impl Assembly {
        fn new<T: Into<Adress>>(entry: T) -> Assembly {
            Assembly {
                entry: entry.into(),
                instructions: vec![],
                data: vec![],
                instruction_labels: HashMap::new(),
                data_labels: HashMap::new(),
            }
        }

        fn raw_bytes(&mut self, buf: Vec<u8>) {
            self.instructions.push(Box::new(buf));
        }

        fn parse_two_operhands(
            &self,
            left: &dyn Operhand,
            right: &dyn Operhand,
            digit: Option<u8>,
        ) -> (Vec<ByteOrLabel>, Vec<ByteOrLabel>) {
            let mut rex: Option<u8> = None;

            let mut index = left.register().unwrap().index();
            if left.rex_64() {
                if index > 0b111 {
                    index = index ^ 0b1000;
                    rex = Some(0b01001100);
                } else {
                    rex = Some(0b01001000);
                }
            }

            let mut modrm = None;
            if let Some(digit) = digit {
                modrm = Some((0b11 << 6) + (digit << 3) + index);
            }
            let mut operhands = vec![];
            if let Some(reg) = right.register() {
                let mut other_reg = reg.index();
                if other_reg > 0b111 {
                    other_reg = other_reg ^ 0b1000;
                    if let Some(rex_) = rex {
                        rex = Some(rex_ | 1);
                    } else {
                        rex = Some(0b01001001)
                    }
                }

                modrm = Some((0b11 << 6) + (index << 3) + other_reg); // Mod rm
            } else if let Some(imm) = right.immediate() {
                let is32 = imm.size == Bitness::_32;
                let bytes = imm.to_bytes();
                let mut as_bytes = vec![];
                if is32 {
                    rex = None;
                }
                for byte in bytes {
                    as_bytes.push(ByteOrLabel::Byte(byte))
                }
                operhands.extend(as_bytes);
            } else if let Some(label) = right.label() {
                operhands.push(ByteOrLabel::Label(label.clone()))
            }

            let mut res = (vec![], vec![]);
            if let Some(rex) = rex {
                res.0.push(ByteOrLabel::Byte(rex));
            }

            if let Some(modrm) = modrm {
                res.1.push(ByteOrLabel::Byte(modrm));
            }
            res.1.extend(operhands);
            res
        }

        fn encode_instructon(args: EncodeArgs) -> Vec<ByteOrLabel> {
            //dbg!(args.clone());
            let mut opcode = args.opcode;
            let digit = args.digit;
            let isr = args.isr;
            let plusreg = args.plusreg;
            let operhandsize = args.operhandsize;
            let left = args.left;
            let right = args.right;

            let mut modrm: Option<u8> = None;
            let mut rex: Option<u8> = None;

            let mut operhands = vec![];
            if let Some(digit) = digit {
                modrm = Some(digit << 3);
                if let Some(reg) = left {
                    if let Some(reg) = reg.register() {
                        let mut index = reg.index();
                        let mut regsize = reg.rex_64();
                        modrm = Some(modrm.unwrap() + (reg.mode() << 6) + index);

                        if let Some(size) = operhandsize {
                            regsize = size == Bitness::_64 || reg.rex_64();
                            let imm_or_label = right.expect("execpeted right side");

                            if let Some(imm) = imm_or_label.immediate() {
                                let bytes = imm.to_bytes_sized(size);
                                let mut as_bytes = vec![];
                                for byte in bytes {
                                    as_bytes.push(ByteOrLabel::Byte(byte))
                                }
                                operhands.extend(as_bytes);
                            } else if let Some(label) = imm_or_label.label() {
                                operhands.push(ByteOrLabel::Label(label.clone()));
                            } else {
                                panic!();
                            }
                        }
                        if regsize || index > 0b111 {
                            rex = Some((0b0100 << 4) + (index >> 3) + ((regsize as u8) << 3));
                            index = index & 0b0111;
                        }
                    } else {
                        panic!("no register but digit was given")
                    }
                } else {
                    panic!("no operhand but digit was given")
                }
            } else if isr {
                let left = left.unwrap().register().unwrap();
                let mut leftindex = left.index();

                let right = right.unwrap().register().unwrap();
                let mut rightindex = right.index();
                let size = left.rex_64() | right.rex_64();
                let mode = right.mode();

                if size || (leftindex > 0b111) || (rightindex > 0b111) {
                    rex = Some(
                        (0b0100 << 4)
                            + ((size as u8) << 3)
                            + ((leftindex >> 3) << 2)
                            + (rightindex >> 3),
                    );
                    leftindex = leftindex & 0b0111;
                    rightindex = rightindex & 0b0111;
                }

                modrm = Some((mode << 6) + (leftindex << 3) + rightindex);

                if let Some(off) = right.offest() {
                    let bytes = off.to_le_bytes().to_vec();
                    for byte in bytes {
                        operhands.push(ByteOrLabel::Byte(byte));
                    }
                }
            } else if plusreg {
                if let Some(reg) = left {
                    if let Some(reg) = reg.register() {
                        let mut index = reg.index();
                        let len = opcode.len();
                        let mut regsize = reg.rex_64();
                        if let Some(size) = operhandsize {
                            regsize = size == Bitness::_64;
                            let imm_or_label = right.expect("execpeted right side");

                            if let Some(imm) = imm_or_label.immediate() {
                                let bytes = imm.to_bytes();
                                let mut as_bytes = vec![];
                                for byte in bytes {
                                    as_bytes.push(ByteOrLabel::Byte(byte))
                                }
                                operhands.extend(as_bytes);
                            } else if let Some(label) = imm_or_label.label() {
                                operhands.push(ByteOrLabel::Label(label.clone()));
                            } else {
                                panic!();
                            }
                        }
                        if regsize || index > 0b111 {
                            rex = Some((0b0100 << 4) + (index >> 3) + ((regsize as u8) << 3));
                            index = index & 0b0111;
                        }
                        opcode[len - 1] += index;
                    } else {
                        panic!("no register but digit was given")
                    }
                } else {
                    panic!("no left but plusreg was true")
                }
            } else if let Some(size) = operhandsize {
                let left = left.unwrap();
                if let Some(label) = left.label() {
                    operhands.push(ByteOrLabel::Label(label.clone()));
                } else if let Some(imm) = left.immediate() {
                    for byte in imm.to_bytes_sized(size) {
                        operhands.push(ByteOrLabel::Byte(byte));
                    }
                }
            }

            let mut res = vec![];
            if let Some(rex) = rex {
                res.push(ByteOrLabel::Byte(rex));
            }
            for byte in opcode {
                res.push(ByteOrLabel::Byte(byte));
            }

            if let Some(modrm) = modrm {
                res.push(ByteOrLabel::Byte(modrm));
            }
            //dbg!(operhands.clone());
            res.extend(operhands);
            res
        }

        fn label<T>(&mut self, label: T) -> Label
        where
            T: Into<String>,
        {
            let l = Label {
                name: label.into(),
                offset: true,
                size: 4,
            };
            println!("{}", self.instructions.len());
            self.instruction_labels
                .insert(l.clone(), self.instructions.len());
            l
        }

        fn get_instruction_label(&mut self, label: Label) -> usize {
            *self
                .instruction_labels
                .get_key_value(&label.into())
                .unwrap()
                .1
        }

        fn add_data_label(&mut self, label: Label, adress: usize) {
            self.data_labels.insert(label, adress);
        }

        fn mov_internal(&mut self, left: Register, right: Box<dyn Operhand>) -> usize {
            let mov = Mov { left, right };
            self.instructions.push(Box::new(mov));
            self.instructions.len() - 1
            // let bytes = mov.to_bytes();
            // print!("{:x?}", bytes)
        }

        fn mov<T, U>(&mut self, op1: T, op2: U) -> usize
        where
            T: Into<Register>,
            U: IntoOperhand + 'static,
        {
            let left = op1.into();
            let right = op2.build();
            self.mov_internal(left, right)
        }

        fn add<T, U>(&mut self, left: T, right: U)
        where
            T: Into<Register>,
            U: IntoOperhand + 'static,
        {
            let add = Add {
                left: left.into(),
                right: right.build(),
            };
            self.instructions.push(Box::new(add));
        }

        fn sub<T, U>(&mut self, left: T, right: U)
        where
            T: Into<Register>,
            U: IntoOperhand + 'static,
        {
            let sub = Sub {
                left: left.into(),
                right: right.build(),
            };
            self.instructions.push(Box::new(sub));
        }

        fn cmp<T, U>(&mut self, left: T, right: U)
        where
            T: Into<Register>,
            U: IntoOperhand + 'static,
        {
            let sub = Cmp {
                left: left.into(),
                right: right.build(),
            };
            self.instructions.push(Box::new(sub));
        }

        fn mult<T, U>(&mut self, left: T, right: U)
        where
            T: Into<Register>,
            U: Into<Register>,
        {
            let mult = Mult {
                left: left.into(),
                right: right.into(),
            };
            self.instructions.push(Box::new(mult));
        }

        fn call(&mut self, op1: Box<dyn IntoOperhand>) -> usize {
            let operhand = op1.build();
            if let Some(_) = operhand.immediate() {
                panic!("cant call on immediant value");
            }
            let call = Call { right: operhand };
            self.instructions.push(Box::new(call));
            self.instructions.len() - 1
        }

        fn start_func<T>(&mut self, name: T, size: u64) -> Label
        where
            T: Into<String>,
        {
            let label = self.label(name);
            self.push("rbp");
            self.mov("rbp", "rsp");
            if size != 0 {
                self.sub("rsp", size);
            }
            label
        }

        fn end_func(&mut self) {
            self.mov("rsp", "rbp");
            self.pop("rbp");
            self.ret()
        }

        fn jump(&mut self, op1: Box<dyn IntoOperhand>) -> usize {
            let mut operhand = op1.build();
            if let Some(_) = operhand.immediate() {
                panic!("cant call on immediant value");
            } else if let Some(label) = operhand.label() {
                operhand = Box::new(Label {
                    name: label.name.clone(),
                    offset: true,
                    size: 4,
                });
            }
            let jump = Jump { left: operhand };
            self.instructions.push(Box::new(jump));
            self.instructions.len() - 1
        }

        fn jnz(&mut self, op1: Box<dyn IntoOperhand>) {
            let mut operhand = op1.build();
            if let Some(_) = operhand.immediate() {
                panic!("cant call on immediant value");
            } else if let Some(label) = operhand.label() {
                operhand = Box::new(Label {
                    name: label.name.clone(),
                    offset: true,
                    size: 4,
                });
            }
            let jnz = Jnz { left: operhand };
            self.instructions.push(Box::new(jnz));
        }

        fn jz(&mut self, op1: Box<dyn IntoOperhand>) {
            let mut operhand = op1.build();
            if let Some(_) = operhand.immediate() {
                panic!("cant call on immediant value");
            } else if let Some(label) = operhand.label() {
                operhand = Box::new(Label {
                    name: label.name.clone(),
                    offset: true,
                    size: 4,
                });
            }
            let jz = Jz { left: operhand };
            self.instructions.push(Box::new(jz));
        }

        fn push<T: Into<Register>>(&mut self, reg: T) {
            let register: Register = reg.into();
            let byte = Assembly::encode_instructon(EncodeArgs {
                opcode: vec![0x50],
                plusreg: true,
                left: Some(&register),
                ..Default::default()
            });
            // let mut iter = byte.iter().cloned();
            // iter.next();
            // let byte: Vec<ByteOrLabel> = iter.collect();
            self.instructions.push(Box::new(byte));
            //self.instructions.push(Box::new(0x50 + register.index()));
        }

        fn pop<T: Into<Register>>(&mut self, reg: T) {
            let register: Register = reg.into();
            let byte = Assembly::encode_instructon(EncodeArgs {
                opcode: vec![0x58],
                plusreg: true,
                left: Some(&register),
                ..Default::default()
            });
            // let mut iter = byte.iter().cloned();
            // iter.next();
            // let byte: Vec<ByteOrLabel> = iter.collect();
            self.instructions.push(Box::new(byte));
        }

        fn ret(&mut self) {
            self.instructions.push(Box::new(0xc3));
        }

        fn syscall(&mut self, call_number: u64, args: Vec<Box<dyn IntoOperhand>>) -> usize {
            self.mov(SYSCALL_ARGS[0], call_number);

            for (i, arg) in args.iter().enumerate() {
                let operhand = arg.build();
                self.mov_internal(SYSCALL_ARGS[i + 1].into(), operhand);
            }

            self.instructions.push(Box::new(vec![0x0f, 0x05]));
            self.instructions.len() - 1
        }

        fn add_data<T, U>(&mut self, data: T, label: U) -> Label
        where
            T: Into<String>,
            U: Into<String>,
        {
            let string: String = data.into();
            let label = Label {
                name: label.into(),
                offset: false,
                size: 8,
            };

            let bytes: Vec<u8> = string.bytes().collect();
            self.add_data_label(label.clone(), self.data.len());
            self.data.extend(bytes);
            label
        }

        // fn get_data<U>(&mut self, label: U)  -> String
        // where
        //     U: Into<String>,
        // {
        //     let label = Label {
        //         name: label.into(),
        //         offset: false,
        //         size: 8,
        //     };

        //     self.data_labels.get(&label).unwrap()
        // }

        //avengers assemble
        fn assemble(&mut self) -> Vec<u8> {
            let mut insruction_map: HashMap<Label, u64> = HashMap::new();
            let mut data_map: HashMap<Label, u64> = HashMap::new();

            let mut data_index = 0;

            let mut res = vec![];
            let mut index = 0;
            for (i, instruct) in self.instructions.iter().enumerate() {
                for byte in instruct.to_bytes(&self) {
                    if let ByteOrLabel::Byte(byte) = byte {
                        res.push(ByteOrLabel::Byte(byte));
                        let x = self.instruction_labels.iter().find_map(|(key, &val)| {
                            if val == i {
                                Some(key)
                            } else {
                                None
                            }
                        });

                        if let Some(label) = x {
                            insruction_map.entry(label.clone()).or_insert(index);
                            //insruction_map.insert(label.clone(), index);
                        }
                        index += 1;
                    } else if let ByteOrLabel::Label(label) = byte {
                        index += label.size as u64;
                        res.push(ByteOrLabel::Label(label));
                    }
                }
            }
            for (i, _) in self.data.iter().enumerate() {
                let x = self.data_labels.iter().find_map(
                    |(key, &val)| {
                        if val == i {
                            Some(key)
                        } else {
                            None
                        }
                    },
                );

                if let Some(label) = x {
                    data_map.insert(label.clone(), data_index);
                }
                data_index += 1;
            }

            let mut final_res = vec![];
            let mut index_final: u64 = 0;
            for (i, byte) in res.iter().enumerate() {
                if let ByteOrLabel::Label(label) = byte {
                    let offset = insruction_map.get(&label);
                    if let Some(offset) = offset {
                        let bytes;
                        if label.offset {
                            let mut adress =
                                (*offset as i32) - (index_final as i32) - (label.size as i32);

                            println!("{}", adress);
                            println!("{}", adress);
                            //let x = from_le_bytes();
                            bytes = adress.to_le_bytes().to_vec();
                            //bytes = vec![0xf8, 0xff, 0xff, 0xff]
                        } else {
                            let adress = self.entry + *offset;
                            bytes = adress.to_bytes();
                        }
                        final_res.extend(bytes);
                    } else {
                        let offset = data_map.get(&label);
                        if let Some(offset) = offset {
                            let adress = self.entry + *offset + index;
                            final_res.extend(adress.to_bytes());
                        } else {
                            panic!("{} is not defined", label.name);
                        }
                    }
                    // - cause we will add one more at the end of the loop
                    index_final += (label.size as u64) - 1;
                } else if let ByteOrLabel::Byte(byte) = byte {
                    final_res.push(*byte);
                }
                index_final += 1;
            }
            final_res.extend(self.data.clone());

            final_res
        }
    }

    pub struct Elf {
        header: Vec<u8>,
        program_header: Vec<u8>,
        code: Vec<u8>,
    }
    const ENTRY: [u8; 8] = {
        let entry: u64 = 0x400078;
        let entry: [u8; 8] = entry.to_le_bytes();
        entry
    };

    pub fn decode_hex(s: &str) -> Result<Vec<u8>, std::num::ParseIntError> {
        (0..s.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&s[i..i + 2], 16))
            .collect()
    }
    pub use crate::ast::ast::{
        AstNode, BinaryOperator, Block, Function, FunctionArgs, FunctionOperator, Literal,
        Operators, VarType, Variable,
    };
    impl Elf {
        fn parse_boperator(
            asm: &mut Assembly,
            node: &AstNode,
            varlocation: &HashMap<String, i32>,
            block: &Block,
            parents: &Vec<Block>,
        ) {
            match node {
                AstNode::Block(_) => panic!(),
                AstNode::Function(_) => panic!(),
                AstNode::If(_, _) => panic!(),
                AstNode::Literal(lit) => {
                    let number: Result<u64, _> = lit.data.parse();
                    if let Ok(number) = number {
                        asm.mov("r10", number);
                        asm.push("r10");
                    } else {
                        let label = asm.add_data(lit.data.clone(), lit.data.clone());
                        asm.mov("r10", label);
                        asm.push("r10");
                    }
                }
                AstNode::BinaryOperator(op) => {
                    if op.type_ == Operators::Set {
                        Elf::parse_boperator(asm, &op.right, varlocation, block, parents);
                        if let AstNode::Identifier(ident) = op.left.as_ref() {
                            let location = *varlocation.get(&ident.name).unwrap();
                            let mut location = location.to_string();
                            if location.chars().next().unwrap() != '-' {
                                location.insert_str(0, "+");
                            }
                            asm.pop("r10");
                            asm.mov(format!("[rbp{}]", location).as_str(), "r10");
                        } else {
                            panic!("no ident after set");
                        }
                    } else {
                        Elf::parse_boperator(asm, &op.right, varlocation, block, parents);
                        Elf::parse_boperator(asm, &op.left, varlocation, block, parents);
                        asm.pop("r10");
                        asm.pop("r11");
                        match op.type_ {
                            Operators::Eq => {
                                asm.cmp("r10", "r11");
                            }
                            Operators::Plus => {
                                asm.add("r10", "r11");
                                asm.push("r10");
                            }
                            Operators::Minus => {
                                asm.sub("r10", "r11");
                                asm.push("r10");
                            }
                            Operators::Set => panic!(),
                            Operators::Divide => todo!(),
                            Operators::Mult => todo!(),
                        }
                    }
                }
                AstNode::FunctionOperator(func) => {
                    if func.name == "return" {
                        for (i, arg) in func.args.iter().enumerate() {
                            let location = *varlocation.get(&format!("return-{}", i)).unwrap();
                            let mut location = location.to_string();
                            if location.chars().next().unwrap() != '-' {
                                location.insert_str(0, "+");
                            }
                            Elf::parse_boperator(asm, arg, varlocation, block, parents);
                            asm.pop("r10");
                            asm.mov(format!("[rbp{}]", location).as_str(), "r10");
                        }
                    } else if func.name == "printnumber" {
                        let size = 16;
                        for arg in &func.args {
                            Elf::parse_boperator(asm, arg, varlocation, block, parents);
                        }
                        asm.pop("rax");
                        let regdumpout = Label::new("regdumpdata");
                        let regdumplabel = Label::new("regdump");
                        asm.mov("rbx", regdumpout.clone());
                        asm.call(Box::new(regdumplabel));

                        asm.syscall(
                            1,
                            vec![Box::new(1), Box::new(regdumpout), Box::new(size as u64)],
                        );
                    } else if func.name == "print" || func.name == "printchar" {
                        let size = {
                            match &func.args[0] {
                                AstNode::Block(_) => 1,
                                AstNode::Function(_) => 1,
                                AstNode::If(_, _) => 1,
                                AstNode::Literal(lit) => match lit.type_ {
                                    VarType::Number => 1,
                                    VarType::Text => lit.data.len() as u64,
                                    VarType::Bool => 1,
                                    VarType::Function => 1,
                                    VarType::Void => 1,
                                    VarType::Any => 1,
                                },
                                AstNode::BinaryOperator(_) => 1,
                                AstNode::FunctionOperator(_) => 1,
                                AstNode::Identifier(_) => 1,
                                AstNode::Unknown => 1,
                            }
                        };

                        for arg in &func.args {
                            Elf::parse_boperator(asm, arg, varlocation, block, parents);
                        }

                        if size == 1 {
                            let regdumpout = Label::new("regdumpdata");
                            asm.pop("r11");
                            asm.mov("r10", regdumpout);
                            asm.mov("[r10]", "r11");
                        } else {
                            asm.pop("r10");
                        }
                        asm.syscall(1, vec![Box::new(1), Box::new("r10"), Box::new(size as u64)]);
                    } else {
                        let func_obj = block.get_func(func.name.clone(), parents).unwrap();
                        asm.sub("rsp", (func_obj.returns.len() as u64) * 8);
                        for arg in &func.args {
                            Elf::parse_boperator(asm, arg, varlocation, block, parents);
                        }
                        let label = Label::new(func.name.clone());
                        asm.call(Box::new(label));
                        asm.add("rsp", (func_obj.args.len() as u64) * 8);
                    }
                }
                AstNode::Identifier(ident) => {
                    let location = *varlocation.get(&ident.name).unwrap();
                    let mut location = location.to_string();
                    if location.chars().next().unwrap() != '-' {
                        location.insert_str(0, "+");
                    }
                    asm.mov("r10", format!("[rbp{}]", location));
                    asm.push("r10");
                }
                AstNode::Unknown => panic!(),
            }
        }

        fn parse_block(
            asm: &mut Assembly,
            block: &Block,
            varlocation: &HashMap<String, i32>,
            parents: &Vec<Block>,
            depth: u64
        ) {
            for node in &block.data {
                match node {
                    AstNode::Block(_) => todo!(),
                    AstNode::Function(func) => {
                        let mut newvarloc: HashMap<String, i32> = HashMap::new();
                        let func_obj = block.get_func(func.name.clone(), parents).unwrap();

                        let mut argsize = (func_obj.args.len() * 8) as i32 + 8;
                        let mut localsize = 0;
                        let inner = &parents[func.inner_block];
                        let mut returnsize = argsize + (func_obj.returns.len() * 8) as i32;
                        for (i, _) in func_obj.returns.iter().enumerate() {
                            newvarloc.insert(format!("return-{}", i), returnsize);
                            returnsize -= 8;
                        }
                        
                        for var in &inner.variables {
                            if var.arg {
                                newvarloc.insert(var.name.clone(), argsize);
                                argsize -= 8;
                            }
                            // } else {
                            //     localsize += 8;
                            //     newvarloc.insert(var.name.clone(), localsize * -1);
                            // }
                        };
                        for var in 


                        asm.start_func(func.name.clone(), localsize as u64);
                        Elf::parse_block(asm, inner, &newvarloc, parents, depth + 1);
                        asm.end_func();
                    }
                    AstNode::If(_, _) => todo!(),
                    AstNode::Literal(_) => todo!(),
                    AstNode::BinaryOperator(_) => {
                        Elf::parse_boperator(asm, node, varlocation, block, parents);
                    }
                    AstNode::FunctionOperator(_) => {
                        Elf::parse_boperator(asm, node, varlocation, block, parents);
                    }
                    AstNode::Identifier(_) => todo!(),
                    AstNode::Unknown => todo!(),
                }
            }
        }

        fn add_tostring(asm: &mut Assembly) {
            let regdumpout = Label::new("regdumpdata");
            let regdumplabel = Label::new("regdump");
            asm.start_func("tostring", 0);
            asm.mov("rbx", regdumpout.clone());
            asm.mov("rax", "[rbp+8]");
            asm.call(Box::new(regdumplabel));
            asm.mov("[rbp+16]", "rbx");
            asm.end_func();
        }

        fn add_print(asm: &mut Assembly) {
            asm.start_func("print", 0);
            asm.syscall(
                1,
                vec![Box::new(1), Box::new("[rbp+16]"), Box::new("[rbp+8]")],
            );
            asm.end_func();
        }

        pub fn new(code: Vec<Block>) -> Elf {
            //rax in rbx out;
            //let regdump = decode_hex("5751524883EC104889E748C7C10A00000048FFCFC607004883F800770748FFCFC60730EB144831D248F7F14883C23048FFCF88174883F80077EA8A0748FFC7880348FFC33C0075F14889D848FFC84883C4105A595FC3").unwrap();
            //let regdumplabel = Label::new("regdump");
            let mut asm = Assembly::new(0x400078);

            let _ = asm.add_data("\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0", "regdumpdata");
            let regdump = "64574883EC104889E748C7C10A00000048FFCFC607004883F800770848FFCFC60730EB154831D248F7F14883C23048FFCF88174883F80077EB8A0748FFC7880348FFC33C0075F24889D848FFC84883C4105FC3";

            let main = Label::new("main");

            asm.call(Box::new(main));
            asm.syscall(60, vec![Box::new(0)]);

            // asm.label("regdump");
            // asm.raw_bytes(regdump);

            let start = &code[1]; //1 not - 0 is global scop
            let mut totalsize: u64 = 0;
            let mut varlocation: HashMap<String, i32> = HashMap::new();

            for var in &start.variables {
                let size = if var.type_ == VarType::Bool { 1 } else { 8 };
                totalsize += size;
                varlocation.insert(var.name.clone(), (totalsize as i32) * -1);
            }
            //asm.start_func("main", totalsize);

            Elf::parse_block(&mut asm, &start, &varlocation, &code, 0 );
            //asm.mov("rax", "[rbp-8]");
            //Elf::print(&mut asm);
            //asm.end_func();
            // Elf::add_print(&mut asm);
            // Elf::add_tostring(&mut asm);
            // asm.label("regdump");
            // asm.raw_bytes(decode_hex(regdump).unwrap());

            let code = asm.assemble();
            Elf::build(code)
        }

        pub fn build(code: Vec<u8>) -> Elf {
            let header = vec![
                0x7f, 'E' as u8, 'L' as u8, 'F' as u8, 0x2, // 64 bit
                0x1, // endians
                0x1, // version
                0x0, // abi
                0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,  //padding
                0x02, // exe type
                0x0,  //padding
                0x3E, // instruction set
                0x0,  // padding
                0x1, 0x0, 0x0, 0x0, // version padding
                ENTRY[0], ENTRY[1], ENTRY[2], ENTRY[3], ENTRY[4], ENTRY[5], ENTRY[6],
                ENTRY[7], //entry adress
                0x40, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, // program header offset
                0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, // section header offest (not present)
                0x0, 0x0, 0x0, 0x0, // flags
                0x40, 0x0, //this headers size
                0x38, 0x0, // program header entry size
                0x1, 0x0, // amount of entries
                0x0, 0x0, // size of section header(not present)
                0x0, 0x0, // amount of sections(not present)
                0x0, 0x0, // index into section header to the section names(not present)
            ];

            let lenght = code.len() as u64;

            let mut lenght: [u8; 8] = lenght.to_be_bytes();
            lenght.reverse();

            let program_header = vec![
                0x1,
                0x0,
                0x0,
                0x0, // load segment
                0x1 | 0x2 | 0x4,
                0x0,
                0x0,
                0x0, // Execute, write and read perms
                0x78,
                0x0,
                0x0,
                0x0,
                0x0,
                0x0,
                0x0,
                0x0,
                ENTRY[0],
                ENTRY[1],
                ENTRY[2],
                ENTRY[3],
                ENTRY[4],
                ENTRY[5],
                ENTRY[6],
                ENTRY[7], //entry adress
                0x0,
                0x0,
                0x0,
                0x0,
                0x0,
                0x0,
                0x0,
                0x0,
                lenght[0],
                lenght[1],
                lenght[2],
                lenght[3],
                lenght[4],
                lenght[5],
                lenght[6],
                lenght[7],
                lenght[0],
                lenght[1],
                lenght[2],
                lenght[3],
                lenght[4],
                lenght[5],
                lenght[6],
                lenght[7],
                0x0,
                0x0,
                0x0,
                0x0,
                0x0,
                0x0,
                0x0,
                0x0,
            ];

            Elf {
                header,
                program_header,
                code,
            }
        }

        pub fn write(mut self) {
            self.header.extend(self.program_header.into_iter());
            self.header.extend(self.code.into_iter());
            if let Err(err) = fs::write("./robin_is_irritant", self.header) {
                println!("{}", err)
            }
        }
    }
}
