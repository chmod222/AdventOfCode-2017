use std::io::{self, BufRead};
use std::fmt::{Display, Formatter, Error};

#[derive(Debug, Clone)]
struct Image {
    data: Vec<Vec<bool>>,
}

impl Image {
    fn from_string(input: &str) -> Option<Image> {
        let parts = input.split('/').map(|l| l.chars().map(|c| c == '#').collect()).collect();

        Some(Image {
            data: parts
        })
    }

    fn from_chunks(input: &Vec<Vec<Image>>) -> Image {
        let blocks = input.len();
        let blocksize = input[0][0].size();

        let mut out = Vec::new();

        for y in 0..blocks*blocksize {
            let mut row = Vec::new();

            for x in 0..blocks*blocksize {
                let block = &input[y / blocksize][x / blocksize];
                let pix = block.data[y % blocksize][x % blocksize];

                row.push(pix);
            }

            out.push(row);
        }

        Image {
            data: out
        }
    }

    fn matches(&self, other: &Image) -> bool {
        if self.size() != other.size() {
            return false;
        }

        for y in 0..self.size() {
            for x in 0..self.size() {
                if self.data[y][x] != other.data[y][x] {
                    return false;
                }
            }
        }

        return true;
    }

    fn size(&self) -> usize {
        self.data.len()
    }

    // Mirror over veritcal axis
    fn mirror(&mut self) {
        let mut data = self.data.clone();

        for row in &mut data {
            row.reverse();
        }

        self.data = data;
    }

    // Rotate clockwise
    fn rotate(&mut self) {
        if self.size() == 2 {
            let tmp = self.data[0][0];

            self.data[0][0] = self.data[1][0];
            self.data[1][0] = self.data[1][1];
            self.data[1][1] = self.data[0][1];
            self.data[0][1] = tmp;

        } else {
            let n = self.size() - 1;

            for x in 0..n / 2 {
                for y in 0..n - x {
                    let tmp = self.data[x][y];

                    self.data[x][y] = self.data[n - y][x];
                    self.data[n - y][x] = self.data[n - x][n - y];
                    self.data[n - x][n - y] = self.data[y][n - x];
                    self.data[y][n - x] = tmp;
                }
            }
        }
    }

    fn subimage(&self, xoff: usize, yoff: usize, size: usize) -> Image {
        let mut subimage = Vec::new();

        for y in yoff..yoff + size {
            subimage.push(self.data[y][xoff..xoff+size].iter().cloned().collect());
        }

        Image {
            data: subimage
        }
    }

    fn to_blocks(&self, size: usize) -> Vec<Vec<Image>> {
        if self.size() % size != 0 {
            panic!("incompatible block size");
        }

        let blocks = self.size() / size;
        let mut output = Vec::new();

        for y in 0..blocks {
            let mut row = Vec::new();

            for x in 0..blocks {
                row.push(self.subimage(x * size, y * size, size));
            }

            output.push(row);
        }

        output
    }

    fn popcount(&self) -> usize {
        self.data.iter().map(|r| r.iter().filter(|&x| *x).count()).sum()
    }
}

impl Display for Image {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        for row in &self.data {
            for col in row {
                write!(f, "{}", if *col { '#' } else { '.' })?;
            }

            writeln!(f, "")?;
        }

        Ok(())
    }
}

struct Rule(Image, Image);

fn attempt_match(pattern: &Image, chunk: &Image) -> bool {
    if pattern.size() != chunk.size() {
        return false;
    }

    let mut pat = pattern.clone();

    for n in 0..8 {
        if n == 4 {
            pat.mirror();
        }

        if pat.matches(chunk) {
            return true;
        }

        pat.rotate();
    }

    false
}

fn replace_chunks(rules: &[Rule], chunks: &mut Vec<Vec<Image>>) {
    for y in 0..chunks.len() {
        for x in 0..chunks.len() {
            for &Rule(ref pat, ref repl) in rules {
                if attempt_match(pat, &chunks[y][x]) {
                    chunks[y][x] = repl.clone();
                    break;
                }
            }
        }
    }
}

fn main() {
    let stdin = io::stdin();
    let rules = stdin
        .lock()
        .lines()
        .filter_map(|l| {
            let line = l.ok()?;
            let mut segments = line.split(" => ");

            let pattern = Image::from_string(segments.next()?)?;
            let output = Image::from_string(segments.next()?)?;

            Some(Rule(pattern, output))
        })
        .collect::<Vec<_>>();

    let mut image = Image::from_string(".#./..#/###").unwrap();

    for i in 0..19 {
        println!("Step {}: {}", i, image.popcount());

        let mut chunks = if image.size() % 2 == 0 {
            image.to_blocks(2)
        } else if image.size() % 3 == 0 {
            image.to_blocks(3)
        } else {
            panic!("...");
        };

        replace_chunks(&rules, &mut chunks);

        image = Image::from_chunks(&chunks);
    }

    let mut wat = Image::from_string("../.#").unwrap();
}
