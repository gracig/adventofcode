use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

pub fn main() {
    process("../sample", find_number_part_1);
    process("../input.txt", find_number_part_1);
    process("../sample2", find_number_part_2);
    process("../input.txt", find_number_part_2);
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum State {
    Initial,
    O,
    On,
    T,
    Tw,
    Th,
    Thr,
    Thre,
    F,
    Fo,
    Fou,
    Fi,
    Fiv,
    S,
    Si,
    Se,
    Sev,
    Seve,
    E,
    Ei,
    Eig,
    Eigh,
    N,
    Ni,
    Nin,
    Digit(char),
    Terminate,
}

pub fn find_number_part_1(s: &str) -> Option<u64> {
    let mut first: Option<u64> = None;
    let mut last: Option<u64> = None;
    for c in s.chars() {
        if c.is_ascii_digit() {
            update_numbers(&mut first, &mut last, c);
        }
    }
    if let (Some(x), Some(y)) = (first, last) {
        return Some(x * 10 + y);
    }
    None
}

pub fn get_transitions() -> HashMap<(&'static State, char), State> {
    HashMap::from([
        ((&State::Initial, 'o'), State::O),
        ((&State::Initial, 't'), State::T),
        ((&State::Initial, 'f'), State::F),
        ((&State::Initial, 's'), State::S),
        ((&State::Initial, 'e'), State::E),
        ((&State::Initial, 'n'), State::N),
        ((&State::Initial, '1'), State::Digit('1')),
        ((&State::Initial, '2'), State::Digit('2')),
        ((&State::Initial, '3'), State::Digit('3')),
        ((&State::Initial, '4'), State::Digit('4')),
        ((&State::Initial, '5'), State::Digit('5')),
        ((&State::Initial, '6'), State::Digit('6')),
        ((&State::Initial, '7'), State::Digit('7')),
        ((&State::Initial, '8'), State::Digit('8')),
        ((&State::Initial, '9'), State::Digit('9')),
        ((&State::O, 'n'), State::On),
        ((&State::On, 'e'), State::Digit('1')),
        ((&State::T, 'w'), State::Tw),
        ((&State::Tw, 'o'), State::Digit('2')),
        ((&State::T, 'h'), State::Th),
        ((&State::Th, 'r'), State::Thr),
        ((&State::Thr, 'e'), State::Thre),
        ((&State::Thre, 'e'), State::Digit('3')),
        ((&State::F, 'o'), State::Fo),
        ((&State::F, 'i'), State::Fi),
        ((&State::Fo, 'u'), State::Fou),
        ((&State::Fou, 'r'), State::Digit('4')),
        ((&State::Fi, 'v'), State::Fiv),
        ((&State::Fiv, 'e'), State::Digit('5')),
        ((&State::S, 'i'), State::Si),
        ((&State::Si, 'x'), State::Digit('6')),
        ((&State::S, 'e'), State::Se),
        ((&State::Se, 'v'), State::Sev),
        ((&State::Sev, 'e'), State::Seve),
        ((&State::Seve, 'n'), State::Digit('7')),
        ((&State::E, 'i'), State::Ei),
        ((&State::Ei, 'g'), State::Eig),
        ((&State::Eig, 'h'), State::Eigh),
        ((&State::Eigh, 't'), State::Digit('8')),
        ((&State::N, 'i'), State::Ni),
        ((&State::Ni, 'n'), State::Nin),
        ((&State::Nin, 'e'), State::Digit('9')),
    ])
}

pub fn find_number_part_2(s: &str) -> Option<u64> {
    let mut first: Option<u64> = None;
    let mut last: Option<u64> = None;
    let mut states: Vec<State> = [].to_vec();
    let transitions = get_transitions();
    let mut collect_number = |digit: char| -> State {
        update_numbers(&mut first, &mut last, digit);
        State::Terminate
    };
    for c in s.chars() {
        states.push(State::Initial); //Starts a new state machine on every char
        for state in states.iter_mut() {
            *state = match state {
                State::Digit(x) => collect_number(*x),
                ref x => transitions
                    .get(&(x, c))
                    .cloned()
                    .unwrap_or(State::Terminate),
            };
        }
        states.retain(|state| *state != State::Terminate); //Remove the terminated state machines
    }
    if let (Some(x), Some(y)) = (first, last) {
        return Some(x * 10 + y);
    }
    None
}

pub fn update_numbers(first: &mut Option<u64>, last: &mut Option<u64>, c: char) {
    if first.is_none() {
        *first = Some(c as u64 - 48)
    }
    *last = Some(c as u64 - 48);
}

pub fn process(file: &str, mut algorithm: impl FnMut(&str) -> Option<u64>) {
    let mut f = BufReader::new(File::open(file).expect("open failed"));
    let mut buf: Vec<u8> = Vec::<u8>::new();
    let mut total: u64 = 0;
    while f.read_until(b'\n', &mut buf).expect("read_until failed") != 0 {
        let s = String::from_utf8(buf).expect("from_utf8 failed");
        if let Some(x) = algorithm(&s) {
            total += x
        } else {
            println!("find_number returned None");
            break;
        }
        buf = s.into_bytes();
        buf.clear();
    }
    println!("Total is: {}", total)
}
