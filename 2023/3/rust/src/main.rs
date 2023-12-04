use std::error::Error;

use rust::{part_1, part_2};



fn main() -> Result<(), Box<dyn Error>>{
    part_1("../sample1.txt")?;
    part_1("../input.txt")?;
    part_2("../sample1.txt")?;
    part_2("../input.txt")?;
   Ok(())
}
