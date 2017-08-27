#![feature(vec_remove_item)]

trait Rows {
    type Row;
    type Point;
    fn columns(&self) -> Self;
    fn boxes(&self) -> Self;
    fn groups(&self) -> Self;
    fn get(&self, index: (usize, usize)) -> &Self::Point;
    fn set(&self, index: (usize, usize), point: Self::Point) -> Self;
    fn peer_values(&self, index: (usize, usize)) -> Vec<Self::Point>;
}

trait Board: Sized {
    fn solve_from(&self, maybe_index: Option<(usize, usize)>) -> Vec<Self>;
    fn solutions(&self) -> Vec<Self>;
    fn print(&self);
}

trait Index: Sized {
    fn prev(&self) -> Option<Self>;
}

impl<T> Rows for Vec<Vec<T>>
    where T: Clone + PartialEq
{
    type Row = Vec<T>;
    type Point = T;

    fn columns(&self) -> Self {
        let mut cols = Vec::new();
        if !self[0].is_empty() {
            cols.push(self.iter().map(|row| row[0].clone()).collect());
            cols.append(&mut self.iter().map(|row| row[1..].to_owned()).collect::<Self>().columns());
        }
        cols
    }

    fn boxes(&self) -> Self {
        let take_box = |x: usize, y: usize, n: usize| -> Self::Row {
            self.iter().map(|row| {
                let (_, new_row) = row.split_at(x);
                new_row.iter().take(n).cloned().collect::<Vec<T>>()
            }).collect::<Self>().columns().iter().map(|row| {
                let (_, new_row) = row.split_at(y);
                new_row.iter().take(n).cloned().collect::<Vec<T>>()
            }).fold(Vec::new(), |mut x, mut y| {
                x.append(&mut y);
                x
            })
        };
        let xs = [0, 3, 6];
        let f = |&x: &usize| -> Self {
            xs.iter().map(|&y| take_box(x, y, 3)).collect()
        };
        xs.iter().map(f).fold(Vec::new(), |mut x, mut y| {
            x.append(&mut y);
            x
        })
    }

    fn groups(&self) -> Self {
        let mut grps = self.clone();
        grps.append(&mut self.columns());
        grps.append(&mut self.boxes());
        grps
    }

    fn get(&self, index: (usize, usize)) -> &Self::Point {
        let (x, y) = index;
        &self[x][y]
    }

    fn set(&self, index: (usize, usize), point: Self::Point) -> Self {
        let (x, y) = index;
        let mut brd = self.clone();
        brd[x][y] = point;
        brd
    }

    fn peer_values(&self, index: (usize, usize)) -> Vec<Self::Point> {
        let indexes = (0..self.len()).map(|x| {
            (0..self[0].len()).map(|y| {
                (x, y)
            }).collect()
        }).collect();
        let peer_indexes = peers(indexes, index);
        peer_indexes.iter().map(|&pid| self.get(pid)).cloned().collect()
    }
}

impl Board for Vec<Vec<u8>> {
    fn solve_from(&self, maybe_index: Option<(usize, usize)>) -> Vec<Self> {
        if let Some(index) = maybe_index {
            let value = self.get(index);
            let is_valid = |v| !self.peer_values(index).contains(v);
            let valid_values = (1..10).filter(|v| !self.peer_values(index).contains(v));
            let new_boards = match value {
                &0 => {
                    valid_values.map(|v| self.set(index, v)).collect::<Vec<Self>>()
                }
                v if is_valid(v) => {
                    vec![self.clone()]
                }
                _ => Vec::new()
            };
            let solutions_list = new_boards.iter().map(|board| board.solve_from(index.prev()));
            solutions_list.fold(Vec::new(), |mut x, mut y| {
                x.append(&mut y);
                x
            })
        } else {
            vec![self.clone()]
        }
    }

    fn solutions(&self) -> Vec<Self> {
        self.solve_from(Some((1, 1)))
    }

    fn print(&self) {
        for row in self {
            println!("{:?}", row);
        }
    }
}

fn peers(rows: Vec<Vec<(usize, usize)>>, index: (usize, usize)) -> Vec<(usize, usize)> {
    let grps = rows.groups();
    let mut prs = grps.iter().filter(|grp| grp.contains(&index)).cloned().fold(Vec::new(), |mut x, mut y| {
        x.append(&mut y);
        x
    });
    prs.sort();
    prs.dedup();
    prs.remove_item(&index);
    prs
}

impl Index for (usize, usize) {
    fn prev(&self) -> Option<Self> {
        match *self {
            (0, 0) => None,
            (x, 0) => Some((x - 1, 8)),
            (x, y) => Some((x, y - 1)),
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_columns() {}
}

macro_rules! board {
    ($($e:expr),*) => {
        {
            let mut res = vec![];
            $(
                res.push($e.to_vec());
            )*
            res
        }
    }
}

fn main() {
    let b = board!(
        [2,3,4,5,6],
        [5,4,3,2,1]
    );
    b.print();
    let c = b.columns();
    c.print();

    let example = vec![
        vec![4, 0, 0, 0, 0, 0, 8, 0, 5],
        vec![0, 3, 0, 0, 0, 0, 0, 0, 0],
        vec![0, 0, 0, 7, 0, 0, 0, 0, 0],
        vec![0, 2, 0, 0, 0, 0, 0, 6, 0],
        vec![0, 0, 0, 0, 8, 0, 4, 0, 0],
        vec![0, 0, 0, 0, 1, 0, 0, 0, 0],
        vec![0, 0, 0, 6, 0, 3, 0, 7, 0],
        vec![5, 0, 0, 2, 0, 0, 0, 0, 0],
        vec![1, 0, 4, 0, 0, 0, 0, 0, 0],
    ];
    example.print();
    println!("===");

    let solutions = example.solutions();
    println!("solutions.len() is {}", solutions.len());
    // for solution in solutions {
    //     solution.print();
    //     println!("===");
    // }
}
