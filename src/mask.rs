use std::fmt;
use std::ops::{BitAnd, BitOr, BitOrAssign};
use std::str::FromStr;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ParseMaskError {
    #[error("empty mask string")]
    Empty,
    #[error("invalid hex character '{0}' in mask")]
    InvalidHex(char),
    #[error("invalid CPU range or ID: {0}")]
    InvalidCpuList(String),
}

/// Represents a set of CPUs, conceptually equivalent to Linux cpumask_t.
/// Implemented as a dynamically-sized bitset (vector of 64-bit words)
/// so there is no fixed limit (can easily handle 4096+ CPUs).
#[derive(Clone, PartialEq, Eq, Default, Hash)]
pub struct CpuMask {
    words: Vec<u64>,
}

impl CpuMask {
    /// Creates an empty mask.
    pub fn new() -> Self {
        Self { words: Vec::new() }
    }

    /// Creates a mask containing the specified CPUs.
    pub fn from_cpus(cpus: impl IntoIterator<Item = usize>) -> Self {
        let mut mask = Self::new();
        for cpu in cpus {
            mask.set(cpu, true);
        }
        mask
    }

    /// Sets or unsets a CPU in the mask.
    pub fn set(&mut self, cpu: usize, val: bool) {
        let word_idx = cpu / 64;
        let bit_idx = cpu % 64;

        if val {
            if word_idx >= self.words.len() {
                self.words.resize(word_idx + 1, 0);
            }
            self.words[word_idx] |= 1u64 << bit_idx;
        } else if word_idx < self.words.len() {
            self.words[word_idx] &= !(1u64 << bit_idx);
            self.trim();
        }
    }

    /// Checks if a CPU is included in the mask.
    pub fn contains(&self, cpu: usize) -> bool {
        let word_idx = cpu / 64;
        let bit_idx = cpu % 64;
        if word_idx < self.words.len() {
            (self.words[word_idx] & (1u64 << bit_idx)) != 0
        } else {
            false
        }
    }

    /// Returns true if the mask contains no CPUs.
    pub fn is_empty(&self) -> bool {
        self.words.iter().all(|&w| w == 0)
    }

    /// Returns the number of CPUs set in this mask.
    pub fn count(&self) -> usize {
        self.words.iter().map(|w| w.count_ones() as usize).sum()
    }

    /// Returns all CPU indices currently set in the mask in ascending order.
    pub fn to_cpus(&self) -> Vec<usize> {
        let mut res = Vec::new();
        for (w_idx, &word) in self.words.iter().enumerate() {
            let mut w = word;
            while w != 0 {
                let bit = w.trailing_zeros() as usize;
                res.push(w_idx * 64 + bit);
                w &= w - 1; // clear lowest bit
            }
        }
        res
    }

    /// Trims trailing zero words to keep representation minimal.
    fn trim(&mut self) {
        while let Some(&0) = self.words.last() {
            self.words.pop();
        }
    }

    /// Parses a Linux kernel hex cpumask string (e.g., "00000001" or "00000000,00000003").
    /// In Linux, groups of up to 32 bits (8 hex digits) are separated by commas,
    /// with the most significant group first.
    pub fn parse_hex(s: &str) -> Result<Self, ParseMaskError> {
        let cleaned = s.trim().trim_start_matches("0x").trim();
        if cleaned.is_empty() {
            return Err(ParseMaskError::Empty);
        }

        // Split by comma
        let chunks: Vec<&str> = cleaned.split(',').map(|c| c.trim()).collect();
        // Each chunk represents 32 bits in hex (up to 8 hex chars).
        // Chunks are ordered from most significant to least significant (big-endian 32-bit words).
        let mut u32_words: Vec<u32> = Vec::new();
        for chunk in chunks {
            if chunk.is_empty() {
                continue;
            }
            let val = u32::from_str_radix(chunk, 16).map_err(|_| {
                chunk
                    .chars()
                    .find(|c| !c.is_ascii_hexdigit())
                    .map(ParseMaskError::InvalidHex)
                    .unwrap_or(ParseMaskError::InvalidHex('?'))
            })?;
            u32_words.push(val);
        }

        u32_words.reverse(); // Now least significant 32-bit word is at index 0.

        // Combine into 64-bit words
        let mut words = Vec::new();
        for chunk in u32_words.chunks(2) {
            let low = chunk[0] as u64;
            let high = if chunk.len() > 1 {
                (chunk[1] as u64) << 32
            } else {
                0
            };
            words.push(low | high);
        }

        let mut mask = Self { words };
        mask.trim();
        Ok(mask)
    }

    /// Formats the mask as a Linux kernel hex string (comma separated every 32 bits / 8 hex digits).
    /// Always outputs at least 8 hex digits (one 32-bit chunk), matching kernel style.
    pub fn to_hex_string(&self) -> String {
        if self.is_empty() {
            return "00000000".to_string();
        }

        // Convert 64-bit words into 32-bit chunks
        let mut u32_words = Vec::new();
        for &w in &self.words {
            u32_words.push((w & 0xFFFF_FFFF) as u32);
            u32_words.push((w >> 32) as u32);
        }

        // Trim trailing zeroes in u32_words
        while u32_words.len() > 1 && *u32_words.last().unwrap() == 0 {
            u32_words.pop();
        }

        // Kernel displays most significant 32-bit chunk first
        u32_words.reverse();
        u32_words
            .iter()
            .map(|w| format!("{:08x}", w))
            .collect::<Vec<_>>()
            .join(",")
    }

    /// Parses a standard Linux CPU list string (e.g., "0-3,7,9-11").
    pub fn parse_cpu_list(s: &str) -> Result<Self, ParseMaskError> {
        let s = s.trim();
        if s.is_empty() {
            return Ok(Self::new());
        }

        let mut mask = Self::new();
        for part in s.split(',') {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }
            if let Some((start_str, end_str)) = part.split_once('-') {
                let start: usize = start_str
                    .trim()
                    .parse()
                    .map_err(|_| ParseMaskError::InvalidCpuList(part.to_string()))?;
                let end: usize = end_str
                    .trim()
                    .parse()
                    .map_err(|_| ParseMaskError::InvalidCpuList(part.to_string()))?;
                if start > end {
                    return Err(ParseMaskError::InvalidCpuList(part.to_string()));
                }
                for cpu in start..=end {
                    mask.set(cpu, true);
                }
            } else {
                let cpu: usize = part
                    .parse()
                    .map_err(|_| ParseMaskError::InvalidCpuList(part.to_string()))?;
                mask.set(cpu, true);
            }
        }
        Ok(mask)
    }

    /// Formats the mask into a CPU list string (e.g. "0-3,7,9-11").
    pub fn to_cpu_list_string(&self) -> String {
        let cpus = self.to_cpus();
        if cpus.is_empty() {
            return String::new();
        }

        let mut ranges = Vec::new();
        let mut range_start = cpus[0];
        let mut prev = cpus[0];

        for &cpu in &cpus[1..] {
            if cpu == prev + 1 {
                prev = cpu;
            } else {
                if range_start == prev {
                    ranges.push(format!("{}", range_start));
                } else {
                    ranges.push(format!("{}-{}", range_start, prev));
                }
                range_start = cpu;
                prev = cpu;
            }
        }

        if range_start == prev {
            ranges.push(format!("{}", range_start));
        } else {
            ranges.push(format!("{}-{}", range_start, prev));
        }

        ranges.join(",")
    }
}

impl fmt::Display for CpuMask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_hex_string())
    }
}

impl fmt::Debug for CpuMask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CpuMask(hex={}, cpus={})", self.to_hex_string(), self.to_cpu_list_string())
    }
}

impl FromStr for CpuMask {
    type Err = ParseMaskError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse_hex(s)
    }
}

impl BitOr for &CpuMask {
    type Output = CpuMask;

    fn bitor(self, rhs: Self) -> Self::Output {
        let max_len = self.words.len().max(rhs.words.len());
        let mut words = Vec::with_capacity(max_len);
        for i in 0..max_len {
            let w1 = self.words.get(i).copied().unwrap_or(0);
            let w2 = rhs.words.get(i).copied().unwrap_or(0);
            words.push(w1 | w2);
        }
        let mut mask = CpuMask { words };
        mask.trim();
        mask
    }
}

impl BitOrAssign<&CpuMask> for CpuMask {
    fn bitor_assign(&mut self, rhs: &CpuMask) {
        *self = &*self | rhs;
    }
}

impl BitAnd for &CpuMask {
    type Output = CpuMask;

    fn bitand(self, rhs: Self) -> Self::Output {
        let min_len = self.words.len().min(rhs.words.len());
        let mut words = Vec::with_capacity(min_len);
        for i in 0..min_len {
            words.push(self.words[i] & rhs.words[i]);
        }
        let mut mask = CpuMask { words };
        mask.trim();
        mask
    }
}

impl Serialize for CpuMask {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_hex_string())
    }
}

impl<'de> Deserialize<'de> for CpuMask {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        CpuMask::parse_hex(&s).map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cpu_mask_basic() {
        let mut mask = CpuMask::new();
        assert!(mask.is_empty());
        assert_eq!(mask.count(), 0);

        mask.set(0, true);
        mask.set(3, true);
        assert_eq!(mask.to_cpus(), vec![0, 3]);
        assert_eq!(mask.to_hex_string(), "00000009");
        assert_eq!(mask.to_cpu_list_string(), "0,3");

        mask.set(3, false);
        assert_eq!(mask.to_cpus(), vec![0]);
        assert_eq!(mask.to_hex_string(), "00000001");
    }

    #[test]
    fn test_cpu_mask_multi_word() {
        let mask = CpuMask::from_cpus([0, 32, 64]);
        assert_eq!(mask.to_cpus(), vec![0, 32, 64]);
        assert_eq!(mask.to_hex_string(), "00000001,00000001,00000001");

        let parsed = CpuMask::parse_hex("00000001,00000001,00000001").unwrap();
        assert_eq!(parsed, mask);
        assert_eq!(parsed.to_cpus(), vec![0, 32, 64]);
    }

    #[test]
    fn test_cpu_list_parsing() {
        let mask = CpuMask::parse_cpu_list("0-3,7,10-12").unwrap();
        assert_eq!(mask.to_cpus(), vec![0, 1, 2, 3, 7, 10, 11, 12]);
        assert_eq!(mask.to_cpu_list_string(), "0-3,7,10-12");
    }
}
