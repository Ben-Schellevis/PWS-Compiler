pub mod elf {
    use std::fs;


    pub trait Instruction {
        fn into_bytes(self) -> Vec<u8>;
    }

    pub struct Elf {
        header: Vec<u8>,
        program_header: Vec<u8>,
        code: Vec<u8>
    }

    impl Elf {
        pub fn new() -> Elf {
            let entry: u64 = 0x400078;
            let mut entry:[u8; 8] = entry.to_be_bytes();
            entry.reverse();
            let header =  vec![
                0x7f, 'E' as u8, 'L' as u8, 'F' as u8,
                0x2, // 64 bit
                0x1, // endians
                0x1, // version
                0x0, // abi

                0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, //padding

                0x02, // exe type
                0x0, //padding
                0x3E, // instruction set
                0x0, // padding
                0x1, 0x0, 0x0, 0x0, // version padding
                entry[0], entry[1], entry[2], entry[3], entry[4], entry[5], entry[6], entry[7], //entry adress
                0x40, 0x0, 0x0, 0x0, 0x0 ,0x0, 0x0, 0x0, // program header offset
                0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, // section header offest (not present)
                0x0, 0x0, 0x0, 0x0, // flags
                0x40, 0x0, //this headers size
                0x38, 0x0, // program header entry size
                0x1, 0x0, // amount of entries
                0x0, 0x0, // size of section header(not present)
                0x0, 0x0, // amount of sections(not present)
                0x0, 0x0 // index into section header to the section names(not present)

                

            ];

            let string: u64 = 0x400078 + 0x31;
            let mut string:[u8; 8] = string.to_be_bytes();
            string.reverse();
            let code = vec![
                0x48, 0xB8, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // rax
                0x48, 0xBF, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // rdi
                0x48, 0xBE, string[0], string[1], string[2], string[3], string[4], string[5], string[6], string[7],// rsi
                0x48, 0xBa, 0x0d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // rdx
                0x0f, 0x05, // syscall

                0x6a, 0x3c, 0x58, 0x31, 0xff, 0x0f, 0x05,

                0x68, 0x65, 0x6c, 0x6c, 0x6f, //helllo
                0x2c, 0x20, //," "
                0x57, 0x6f, 0x72,  0x6c, 0x64, // world
                0x0a, // enter
            ];

            let lenght = code.len() as u64;

            let mut lenght:[u8; 8] = lenght.to_be_bytes();
            lenght.reverse();

            let program_header = vec![
                0x1, 0x0, 0x0, 0x0, // load segment
                0x1 | 0x2 | 0x4, 0x0, 0x0 , 0x0, // Execute, write and read perms
                0x78, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
                entry[0], entry[1], entry[2], entry[3], entry[4], entry[5], entry[6], entry[7], //entry adress
                0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
                lenght[0], lenght[1], lenght[2],lenght[3], lenght[4], lenght[5], lenght[6], lenght[7],
                lenght[0], lenght[1], lenght[2],lenght[3], lenght[4], lenght[5], lenght[6], lenght[7],
                0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            ];



            Elf {
                header,
                program_header,
                code
            }
        }


        pub fn write(mut self) {
            self.header.extend(self.program_header.into_iter());
            self.header.extend(self.code.into_iter());
            if let Err(err) = fs::write("./robin_is_irriant", self.header) {
                println!("{}", err)
            }
        }
    }
}
