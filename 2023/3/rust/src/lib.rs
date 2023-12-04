use std::{
    error::Error,
    fs::File,
    io::{self, BufRead},
    path::Path,
};

use quadtree_rs::{
    area::{Area, AreaBuilder},
    point::Point,
    Quadtree,
};

pub fn part_1(filename: &str) -> Result<(), Box<dyn Error>> {
    let model = load(filename)?;
    println!("Result is: {}", model.sum_part_numbers());
    Ok(())
}
pub fn part_2(filename: &str) -> Result<(), Box<dyn Error>> {
    let model = load(filename)?;
    println!("Result is: {}", model.multiply_geared_part_numbers());
    Ok(())
}

pub enum State {
    BOL,
    DOT,
    CollectingPartNumber { col: u64, value: String },
    PartNumber { col: u64, value: String },
    Symbol { col: u64, value: char },
}

#[derive(Debug)]
pub struct PartNumber {
    area: Area<i64>,
    value: u64,
}
#[derive(Debug)]
pub struct Symbol {
    pub location: Point<i64>,
    pub value: String,
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

pub fn adjust_width(x: u64, length: u64) -> (u64, u64) {
    if x > 0 {
        (x - 1, length + 3)
    } else {
        (x, length)
    }
}
pub fn adjust_height(y: u64) -> (u64, u64) {
    if y > 0 {
        (y - 1, 3)
    } else {
        (y, 2)
    }
}
#[derive(Debug)]
pub enum Item {
    Symbol(Symbol),
    PartNumber(PartNumber),
}

pub fn insert_part(
    x: i64,
    y: i64,
    buffer: &str,
    part_numbers: &mut Vec<PartNumber>,
    quadtree: &mut Quadtree<i64, Item>,
) -> Result<(), Box<dyn Error>> {
    if buffer.is_empty() {
        return Ok(());
    }
    let w = buffer.len() as i64;
    let h = 1;
    let area = area_with_distance(x, y, w, h, 1)?;
    let value = buffer.parse::<u64>()?;
    part_numbers.push(PartNumber { area, value });
    quadtree.insert(
        AreaBuilder::default()
            .anchor(Point { x, y })
            .dimensions((w, 1))
            .build()?,
        Item::PartNumber(PartNumber { area, value }),
    );
    Ok(())
}

pub fn area_with_distance(x: i64, y: i64, w: i64, h: i64, d: i64) -> Result<Area<i64>, String> {
    let x1 = x - d;
    let y1 = y - d;
    let x2 = w + d * 2;
    let y2 = h + d * 2;
    //println!("{},{} {},{} {},{} {},{}", x,y, w,h, x1, y1, x2, y2);
    AreaBuilder::default()
        .anchor(Point { x: x1, y: y1 })
        .dimensions((x2, y2))
        .build()
}
pub fn insert_symbol(
    x: i64,
    y: i64,
    value: &char,
    symbols: &mut Vec<Symbol>,
    quadtree: &mut Quadtree<i64, Item>,
) -> Result<(), Box<dyn Error>> {
    symbols.push(Symbol {
        location: Point { x, y },
        value: value.to_string(),
    });
    quadtree.insert_pt(
        Point { x, y },
        Item::Symbol(Symbol {
            location: Point { x, y },
            value: value.to_string(),
        }),
    );
    Ok(())
}

pub struct Model {
    pub quadtree: Quadtree<i64, Item>,
    pub part_numbers: Vec<PartNumber>,
    pub symbols: Vec<Symbol>,
}

impl Model {
    pub fn sum_part_numbers(&self) -> u64 {
        self.part_numbers
            .iter()
            .filter_map(|p| {
                let query = self.quadtree.query(p.area);
                let symbols: Vec<&Item> = query
                    .filter(|e| matches!(e.value_ref(), Item::Symbol(_)))
                    .map(|e| e.value_ref())
                    .collect();
                if !symbols.is_empty() {
                    Some((p, symbols))
                } else {
                    None
                }
            })
            .fold(0u64, |acc, (p, _symbols)| {
                println!("{:?}, {:?}", p, _symbols);
                acc + p.value
            })
    }
    pub fn multiply_geared_part_numbers(&self) -> u64 {
        self.symbols
            .iter()
            .filter(|s| s.value == "*")
            .filter_map(|p| {
                let new_point = Point {
                    x: p.location.x - 1,
                    y: p.location.y - 1,
                };
                let area = AreaBuilder::default()
                    .anchor(new_point)
                    .dimensions((3, 3))
                    .build()
                    .unwrap();
                let query = self.quadtree.query(area);
                let part_numbers: Vec<&Item> = query
                    .filter(|e| matches!(e.value_ref(), Item::PartNumber(_)))
                    .map(|e| e.value_ref())
                    .collect();
                if part_numbers.len() > 1 {
                    Some((p, part_numbers))
                } else {
                    None
                }
            })
            .fold(0u64, |acc, (s, _part_numbers)| {
                println!("{:?}, {:?}", s, _part_numbers);
                acc + _part_numbers.iter().fold(1, |acc, p| {
                    if let Item::PartNumber(p) = p {
                        acc * p.value
                    } else {
                        acc
                    }
                })
            })
    }
}

pub fn load(filename: &str) -> Result<Model, Box<dyn Error>> {
    let mut quadtree: Quadtree<i64, Item> = Quadtree::new(10);
    let mut part_numbers: Vec<PartNumber> = vec![];
    let mut symbols: Vec<Symbol> = vec![];
    let mut pbuf = "".to_string();
    let mut pbuf_start = 0;
    for (y, line) in read_lines(filename)?.map_while(Result::ok).enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c.is_ascii_digit() {
                if pbuf.is_empty() {
                    pbuf_start = x;
                }
                pbuf.push(c)
            } else {
                insert_part(
                    pbuf_start as i64,
                    y as i64,
                    &pbuf,
                    &mut part_numbers,
                    &mut quadtree,
                )?;
                pbuf.clear();
                if c != '.' {
                    insert_symbol(x as i64, y as i64, &c, &mut symbols, &mut quadtree)?;
                }
            }
        }
    }
    Ok(Model {
        quadtree,
        part_numbers,
        symbols,
    })
}
