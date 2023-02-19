pub mod elf {
    use std::{
        collections::HashMap, fmt::Debug, fs, hash::Hash, num::NonZeroI8, ops::Add as NumAdd,
    };
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
            let first_char = self.as_bytes()[0] as char;
            let bits = match first_char.to_ascii_lowercase() {
                'r' => Bitness::_64,
                'e' => Bitness::_32,
                _ => panic!("expected r or e"),
            };
            Register {
                bits,
                data: self.into(),
            }
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
    }
    impl Register {
        fn mode(&self) -> u8 {
            0b11
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
    }

    impl IntoOperhand for u64 {
        fn build(&self) -> Box<dyn Operhand> {
            let mut size = Bitness::_64;
            if *self < i32::MAX as u64 {
                size = Bitness::_32;
            }
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
            let label = self.right.label().is_some();
            let opcode: u8 = if imm || label { 0xB8 } else { 0x8B };
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
                left: Some(&self.left),
                right: Some(self.right.as_ref()),
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
            let mut res = vec![ByteOrLabel::Byte(0xE9)];
            if let Some(imm) = self.left.immediate() {
                let bytes = imm.to_bytes();
                for byte in bytes {
                    res.push(ByteOrLabel::Byte(byte));
                }
            } else if let Some(label) = self.left.label() {
                let new_label = Label {
                    name: label.name.clone(),
                    offset: true,
                    size: 4,
                };
                res.push(ByteOrLabel::Label(new_label));
            }
            //res
            //let mut new_left = self.left.as_ref();
            let res;
            if let Some(label) = self.left.label() {
                let new_label = Label {
                    name: label.name.clone(),
                    offset: true,
                    size: 4,
                };

                res = Assembly::encode_instructon(EncodeArgs {
                    opcode: vec![0xE9],
                    operhandsize: Some(Bitness::_32),
                    left: Some(new_label.build().as_ref()),
                    ..Default::default()
                });
            }else{
                res = Assembly::encode_instructon(EncodeArgs {
                    opcode: vec![0xE9],
                    operhandsize: Some(Bitness::_32),
                    left: Some(self.left.as_ref()),
                    ..Default::default()
                });
            }

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
            let opcode = if imm { vec![0x81] } else { vec![0x03] };
            let digit = if imm { Some(0) } else { None };
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

    struct Sub {
        left: Register,
        right: Box<dyn Operhand>,
    }

    impl Instruction for Sub {
        fn to_bytes(&self, asm: &Assembly) -> Vec<ByteOrLabel> {
            let imm = self.right.immediate().is_some();
            let opcode = if imm { vec![0x81] } else { vec![0x2B] };
            let digit = if imm { Some(5) } else { None };
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

    impl<T: Operhand> Operhand for &T{}

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
            dbg!(args.clone());
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
                    } else {
                        panic!("no register but digit was given")
                    }
                } else {
                    panic!("no operhand but digit was given")
                }
            }
            else if isr {
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
            }
            else if plusreg {
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
            }
            else if let Some(_) = operhandsize {
                let left = left.unwrap(); 
                if let Some(label) =left.label(){
                    operhands.push(ByteOrLabel::Label(label.clone()));
                }else if let Some(imm) = left.immediate(){
                    for byte in imm.to_bytes() {
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
            dbg!(operhands.clone());
            res.extend(operhands);
            res
        }

        fn label<T>(&mut self, label: T) -> Label
        where
            T: Into<String>,
        {
            let l = Label {
                name: label.into(),
                offset: false,
                size: 8,
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

        fn jump(&mut self, op1: Box<dyn IntoOperhand>) -> usize {
            let operhand = op1.build();
            if let Some(_) = operhand.immediate() {
                panic!("cant call on immediant value");
            }
            let call = Jump { left: operhand };
            self.instructions.push(Box::new(call));
            self.instructions.len() - 1
        }

        fn push<T: Into<Register>>(&mut self, reg: T) {
            let register: Register = reg.into();
            self.instructions.push(Box::new(0x50 + register.index()));
        }

        fn pop<T: Into<Register>>(&mut self, reg: T) {
            let register: Register = reg.into();
            self.instructions.push(Box::new(0x58 + register.index()));
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

    impl Elf {
        pub fn new() -> Elf {
            let mut asm = Assembly::new(0x400078);

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

            let string: u64 = 0x400078 + 0x36;
            let mut string: [u8; 8] = string.to_be_bytes();
            string.reverse();

            let func: u64 = 0x400078 + 0x3e;
            let mut func: [u8; 8] = func.to_be_bytes();
            func.reverse();
            /*
            let code = vec![
                /*00 */ 0x48, 0xB8, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, // rax
                /*10 */ 0x48, 0xBF, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, // rdi
                /*20 */ 0x48, 0xBE, string[0], string[1], string[2], string[3], string[4],
                string[5], string[6], string[7], // rsi
                /*30 */ 0x48, 0xBa, 0x0d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, // rdx
                /*40 */ 0x0f, 0x05, // syscall
                /*42 */ 0xe8, 0x14, 0x00, 0x00, 0x00, 0x6a, 0x3c, 0x58, 0x31, 0xff, 0x0f,
                0x05, 0x68, 0x65, 0x6c, 0x6c, 0x6f, //helllo
                0x2c, 0x20, //," "
                0x57, 0x6f, 0x72, 0x6c, 0x64, // world
                0x0a, // enter
                0x55, //push rbp
                0x48, 0x8b, 0xec, //mov rbp, rsp
                0xC9, //leaves
                0xC3, // ret
            ];
            */

            let data = asm.add_data("\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0", "data");
            let regdump = Label::new("regdump");
            let exit = Label::new("exit");
            asm.mov("rbx", data.clone());
            asm.mov("rax", 10);
            asm.mov("rdx", 10);
            asm.cmp("rax", "rdx");
            asm.jump(Box::new(exit));
            asm.call(Box::new(regdump));

            asm.syscall(1, vec![Box::new(1), Box::new(data), Box::new(16 as u64)]);


            asm.label("exit");
            asm.syscall(60, vec![Box::new(0)]);

            let regdump = "64574883EC104889E748C7C10A00000048FFCFC607004883F800770848FFCFC60730EB154831D248F7F14883C23048FFCF88174883F80077EB8A0748FFC7880348FFC33C0075F24889D848FFC84883C4105FC3";

            asm.label("regdump");
            asm.raw_bytes(decode_hex(regdump).unwrap());
            //asm.mov("rdx", 10);
            let code = asm.assemble();

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
