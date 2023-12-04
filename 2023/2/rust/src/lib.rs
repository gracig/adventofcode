use std::{
    error::Error,
    fs::File,
    io::{self, BufRead},
    path::Path,
};

use parser::{Color, Game, Subset};

pub mod parser;

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

pub fn print_results(games: &[Game]) {
    println!(
        "Results: {}",
        games.iter().fold(0, |acc, game| {
            println!("Game {}: Red {}, Green {}, Blue {}",game.id, game.max_red(), game.max_green(), game.max_blue() );
            acc + game.id
        })
    )
}

pub fn sum_game_ids(games: &[Game]) -> u32 {
    games.iter().fold(0, |acc, game| acc + game.id)
}

pub fn process_part_1(filename: &str) -> Result<(), Box<dyn Error>> {
    let max_values = Subset::from([(Color::GREEN, 13), (Color::BLUE, 14), (Color::RED, 12)]);
    let games: Vec<Game> = read_lines(filename)?
        .map_while(Result::ok)
        .filter_map(|l| Game::try_from(l.as_str()).ok())
        .filter(|g| {
            g.subsets.iter().all(|s| {
                s.iter().all(|(color, total)| {
                    if let Some(t2) = max_values.get(color) {
                        *total <= *t2
                    } else {
                        true
                    }
                })
            })
        })
        .collect();
    print_results(&games);
    Ok(())
}

pub fn process_part_2(filename: &str) -> Result<(), Box<dyn Error>> {
    let result = read_lines(filename)?
        .map_while(Result::ok)
        .filter_map(|l| Game::try_from(l.as_str()).ok())
        .fold(0, |acc, game| acc + game.power());
    println!("Result is : {}", result);
    Ok(())
}
