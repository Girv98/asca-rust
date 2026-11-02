use serde::{Deserialize, Serialize};


/// Represents the class nodes of a segment
/// # Bit Layout
/// 
/// `111_111_1111111111`
/// 
/// Bits are encoded as follows:
/// 
/// * `Root` - `[cons, son, syll]`
/// * `Laryngeal` - `[voice, sg, cg]`
/// * `Manner` - `[continuent, approx., lateral, nasal, del.rel., strident, rhotic, click, double, _]`
#[derive(Default, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub struct Class(u16);

impl std::ops::Deref for Class {
    type Target = u16;

    fn deref(&self) -> &Self::Target {
       &self.0
    }
}

impl std::ops::DerefMut for Class {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Class {
    const RUT_LOW: u16 = 0b111_000_0000000000; 
    const LAR_LOW: u16 = 0b000_111_0000000000; 
    const MAN_LOW: u16 = 0b000_000_1111111111; 

    const RUT_OFF: u16 = 13; 
    const LAR_OFF: u16 = 10;
    // const MAN_OFF: u16 =  0;

    const RUT_MSK: u16 = 0b111; 
    const LAR_MSK: u16 = 0b111; 
    const MAN_MSK: u16 = 0b1111111111; 

    pub fn get_inner(&self) -> u16 { self.0 }
    pub fn set_inner(&mut self, val: u16) { self.0 = val; }

    pub fn get_root(&self) -> u8 {
        ((self.0 >> Self::RUT_OFF) & Self::RUT_MSK) as u8
    }

    pub fn get_laryngeal(&self) -> u8 {
        ((self.0 >> Self::LAR_OFF) & Self::LAR_MSK) as u8
    }

    pub fn get_manner(&self) -> u16 {
        self.0 & Self::MAN_MSK
    }


    pub fn set_root(&mut self, bits: u8) {
        self.0 = (self.0 & !Self::RUT_LOW) | ((bits as u16) << Self::RUT_OFF)
    }

    pub fn set_laryngeal(&mut self, bits: u8) {
        self.0 = (self.0 & !Self::LAR_LOW) | ((bits as u16) << Self::LAR_OFF)
    }

    pub fn set_manner(&mut self, bits: u16) {
        debug_assert!(bits <= Self::MAN_MSK);
        self.0 = (self.0 & !Self::MAN_LOW) | bits
    }
}