extern crate regex;

use std::ops::{Add, AddAssign};
use std::io::{self, BufRead};
use std::collections::HashMap;

use regex::Regex;


#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
struct Vec3<T>(T, T, T);

impl<T: Add<Output = T> + Copy> Add for Vec3<T> {
    type Output = Vec3<T>;

    fn add(self, other: Vec3<T>) -> Vec3<T> {
        Vec3(self.0 + other.0, self.1 + other.1, self.2 + other.2)
    }
}

impl<T: Add<Output = T> + Copy> AddAssign for Vec3<T> {
    fn add_assign(&mut self, other: Vec3<T>) {
        *self = *self + other;
    }
}

#[derive(Debug, Copy, Clone)]
struct Particle {
    id: usize,

    pos: Vec3<i64>,
    vel: Vec3<i64>,
    acc: Vec3<i64>
}

impl Particle {
    fn tick(&mut self) {
        self.vel += self.acc;
        self.pos += self.vel;
    }

    fn distance(&self) -> i64 {
        (self.pos.0.abs() + self.pos.1.abs() + self.pos.2.abs())
    }
}

fn parse_vec(vec: &str) -> Option<Vec3<i64>> {
    let re = Regex::new(r"[pva]=<(\-?\d+),(\-?\d+),(\-?\d+)>").ok()?;
    let caps = re.captures(vec)?;

    Some(
        Vec3(
            caps[1].parse().ok()?,
            caps[2].parse().ok()?,
            caps[3].parse().ok()?))
}

fn parse_particle(id: usize, ser: &str) -> Option<Particle> {
    let mut segments = ser.split(", ");

    Some(Particle {
        id,
        pos: parse_vec(segments.next()?)?,
        vel: parse_vec(segments.next()?)?,
        acc: parse_vec(segments.next()?)?
    })
}

fn handle_collisions(particles: &mut Vec<Particle>) {
    let mut positions = HashMap::new();

    for particle in particles.iter() {
        let entry = positions.entry(particle.pos).or_insert(0);

        *entry += 1;
    }

    particles.retain(|p| positions[&p.pos] == 1);
}

fn main() {
    let stdin = io::stdin();

    let mut particles = stdin
        .lock()
        .lines()
        .enumerate()
        .filter_map(|(i, l)| l.ok().and_then(|li| parse_particle(i, &li)))
        .collect::<Vec<_>>();

    let mut particles_cpy = particles.clone();

    const STOP_AFTER: usize = 5000;

    let mut since_closest = 0;
    let mut closest = 0;
    let mut range = 0;

    loop {
        for (j, particle) in particles.iter().enumerate() {
            if particle.distance() <= particles[closest].distance()
                && particle.id != particles[closest].id {

                closest = j;
                since_closest = 0;
            }
        }

        if since_closest > STOP_AFTER {
            println!("Part 1: Giving up at iteration {} - closest: {}",
                range,
                particles[closest].id);

            break;
        }

        for particle in &mut particles {
            particle.tick();
        }

        since_closest += 1;
        range += 1;
    }

    for _ in 0..range {
        handle_collisions(&mut particles_cpy);

        for particle in &mut particles_cpy {
            particle.tick();
        }
    }

    println!("Part 2: Particles left: {}", particles_cpy.len());
}
