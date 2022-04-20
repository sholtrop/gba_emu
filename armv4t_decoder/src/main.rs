use crate::decode_arm::Decoder;

mod decode_arm;
mod decode_thumb;
mod register;

fn main() {
    println!("{:?}", Decoder::decode(0xFFFF));
}
