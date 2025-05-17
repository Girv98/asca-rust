use std::fmt;

use serde::{Deserialize, Serialize};

/// Represents place of articulation
/// Is `none` when place is glottalic i.e. `[-place]`
/// # Bit Layout
/// 
/// `1111_11_11_111111_11`
/// 
/// First 4 bits encode which sub nodes are present `[Labial, Coronal, Dorsal, Pharyngeal]`.
/// 
/// The rest are as follows:
/// 
/// * `Labial` - `[labiodental, round]`
/// * `Coronal` -  `[anterior, distributed]`
/// * `Dorsal` -  `[front, back, high, low, tense, reduced]`
/// * `Pharyngeal` - `[atr, rtr]`
#[derive(Default, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub struct Place(Option<u16>);

impl fmt::Debug for Place {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn u8_to_str(f: &mut fmt::Formatter<'_>, num: u8, len: u8) -> fmt::Result {
            for i in (0..len).rev() {
                match num & (1 << i) != 0 {
                    true => write!(f, "+", )?,
                    false => write!(f, "-", )?,
                }
            }
            Ok(())
        }

        match self.get_labial() {
            Some(lab) => u8_to_str(f, lab, 2)?,
            None => write!(f, "00")?
        }
        write!(f, "|")?;
        match self.get_coronal() {
            Some(cor) => u8_to_str(f, cor, 2)?,
            None => write!(f, "00")?
        }
        write!(f, "|")?;
        match self.get_dorsal() {
            Some(dor) => u8_to_str(f, dor, 6)?,
            None => write!(f, "000000")?
        }
        write!(f, "|")?;
        match self.get_pharyngeal() {
            Some(phr) => u8_to_str(f, phr, 2),
            None => write!(f, "00"),
        }
    }
}

impl std::ops::Deref for Place {
    type Target = Option<u16>;

    fn deref(&self) -> &Self::Target {
       &self.0
    }
}

impl std::ops::DerefMut for Place {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Place {
    const LAB_BIT: u16 = 0x8000;
    const COR_BIT: u16 = 0x4000;
    const DOR_BIT: u16 = 0x2000;
    const PHR_BIT: u16 = 0x1000;

    const LAB_LOW: u16 = 0xc00;
    const COR_LOW: u16 = 0x300;
    const DOR_LOW: u16 = 0x0FC;
    const PHR_LOW: u16 = 0x003;

    const LAB_OFF: u8 = 10;
    const COR_OFF: u8 = 8;
    const DOR_OFF: u8 = 2;

    const LAB_MSK: u16 = 0x03;
    const COR_MSK: u16 = 0x03;
    const DOR_MSK: u16 = 0x3F;
    const PHR_MSK: u16 = 0x03;

    /// Returns whether place is a `Some` value
    pub fn is_some(&self) -> bool {
        self.0.is_some()
    }

    /// Returns whether place is a `None` value
    pub fn is_none(&self) -> bool {
        self.0.is_none()
    }

    /// Returns whether the labial subnode is a `Some` value
    pub fn labial_is_some(&self) -> bool {
        match self.0 {
            Some(x) => Self::LAB_BIT & x == Self::LAB_BIT,
            None => false,
        }
    }

    /// Returns whether the labial subnode is a `None` value
    pub fn labial_is_none(&self) -> bool {
        !self.labial_is_some()
    }

    /// Returns whether the coronal subnode is a `Some` value
    pub fn coronal_is_some(&self) -> bool {
        match self.0 {
            Some(x) => Self::COR_BIT & x == Self::COR_BIT,
            None => false,
        }
    }

    /// Returns whether the coronal subnode is a `None` value
    pub fn coronal_is_none(&self) -> bool {
        !self.coronal_is_some()
    }

    /// Returns whether the dorsal subnode is a `Some` value
    pub fn dorsal_is_some(&self) -> bool {
        match self.0 {
            Some(x) => Self::DOR_BIT & x == Self::DOR_BIT,
            None => false,
        }
    }
    
    /// Returns whether the dorsal subnode is a `None` value
    pub fn dorsal_is_none(&self) -> bool {
        !self.dorsal_is_some()
    }
    
    /// Returns whether the pharyngeal subnode is a `Some` value
    pub fn pharyngeal_is_some(&self) -> bool {
        match self.0 {
            Some(x) => Self::PHR_BIT & x == Self::PHR_BIT,
            None => false,
        }
    }
    
    /// Returns whether the pharyngeal subnode is a `None` value
    pub fn pharyngeal_is_none(&self) -> bool {
        !self.pharyngeal_is_some()
    }

    /// Returns value of the labial subnode
    pub fn get_labial(&self) -> Option<u8> {
        if self.labial_is_some() {
            Some(((unsafe { self.0.unwrap_unchecked() } >> Self::LAB_OFF) & Self::LAB_MSK) as u8)
        } else {
            None
        }
    }

    /// Returns value of the coronal subnode
    pub fn get_coronal(&self) -> Option<u8> {
        if self.coronal_is_some() {
            Some(((unsafe { self.0.unwrap_unchecked() } >> Self::COR_OFF) & Self::COR_MSK)as u8)
        } else {
            None
        }
    }

    /// Returns value of the dorsal subnode
    pub fn get_dorsal(&self) -> Option<u8> {
        if self.dorsal_is_some() {
            Some(((unsafe { self.0.unwrap_unchecked() } >> Self::DOR_OFF) & Self::DOR_MSK) as u8)
        } else {
            None
        }
    }

    /// Returns value of the pharyngeal subnode
    pub fn get_pharyngeal(&self) -> Option<u8> {
        if self.pharyngeal_is_some() {
            Some((unsafe { self.0.unwrap_unchecked() } & Self::PHR_MSK) as u8)
        } else {
            None
        }
    }

    /// Sets the labial subnode to the input value 
    /// # Panics
    /// Mask values above 3 `0b11` are not used and will panic in debug
    pub fn set_labial(&mut self, bits: Option<u8>) {
        debug_assert!(bits <= Some(Self::LAB_MSK as u8), "Only lower two bits are used");
        match bits {
            Some(bits) => if let Some(num) = &mut self.0 {
                *num |= Self::LAB_BIT;
                *num = (*num & !Self::LAB_LOW) | ((bits as u16) << Self::LAB_OFF);
            } else {
                self.0 = Some(Self::LAB_BIT | ((Self::LAB_MSK & bits as u16) << Self::LAB_OFF))
            },
            None => if let Some(num) = &mut self.0 {
                *num &= !(Self::LAB_BIT | Self::LAB_LOW)
            },
        }

        if matches!(self.0, Some(0)) { self.0 = None; } 
    }

    /// Sets the coronal subnode to the input value 
    /// # Panics
    /// Mask values above 3 `0b11` are not used and will panic in debug
    pub fn set_coronal(&mut self, bits: Option<u8>) {
        debug_assert!(bits <= Some(Self::COR_MSK as u8), "Only lower two bits are used");
        match bits {
            Some(bits) => if let Some(num) = &mut self.0 {
                *num |= Self::COR_BIT;
                *num = (*num & !Self::COR_LOW) | ((bits as u16) << Self::COR_OFF);
            } else {
                self.0 = Some(Self::COR_BIT | ((Self::COR_MSK & bits as u16) << Self::COR_OFF))
            },
            None => if let Some(num) = &mut self.0 {
                *num &= !(Self::COR_BIT | Self::COR_LOW)
            },
        }
        if matches!(self.0, Some(0)) { self.0 = None; } 
    }

    /// Sets the dorsal subnode to the input value 
    /// # Panics
    /// Mask values above 63 `0b111111` are not used and will panic in debug 
    pub fn set_dorsal(&mut self, bits: Option<u8>) {
        debug_assert!(bits <= Some(Self::DOR_MSK as u8), "Only lower six bits are used");
        match bits {
            Some(bits) => if let Some(num) = &mut self.0 {
                *num |= Self::DOR_BIT;
                *num = (*num & !Self::DOR_LOW) | ((bits as u16) << Self::DOR_OFF);
            } else {
                self.0 = Some(Self::DOR_BIT | ((Self::DOR_MSK & bits as u16) << Self::DOR_OFF))
            },
            None => if let Some(num) = &mut self.0 {
                *num &= !(Self::DOR_BIT | Self::DOR_LOW)
            },
        }
        if matches!(self.0, Some(0)) { self.0 = None; } 
    }
    
    /// Sets the pharyngeaal subnode to the input value 
    /// # Panics
    /// Mask values above 3 `0b11` are not used and will panic in debug
    pub fn set_pharyngeal(&mut self, bits: Option<u8>) {
        debug_assert!(bits <= Some(Self::PHR_MSK as u8), "Only lower two bits are used");
        match bits {
            Some(bits) => if let Some(num) = &mut self.0 {
                *num |= Self::PHR_BIT;
                *num = (*num & !0x03) | bits as u16;
            } else {
                self.0 = Some(Self::PHR_BIT | (Self::PHR_MSK & bits as u16))
            },
            None => if let Some(num) = &mut self.0 {
                *num &= !(Self::PHR_BIT | Self::PHR_LOW)
            },
        }
        if matches!(self.0, Some(0)) { self.0 = None; } 
    }
}