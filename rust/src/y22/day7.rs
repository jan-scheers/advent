pub const INPUT: &str = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq)]
struct Path {
    name: String,
    kind: PathKind,
}

#[derive(Debug, PartialEq, Eq)]
enum PathKind {
    Dir(Vec<Rc<RefCell<Path>>>),
    File(usize),
}

impl Path {
    fn recurse(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
        let description = match self.kind {
            PathKind::Dir(_) => "(dir)".to_string(),
            PathKind::File(size) => format!("(file, size={})", size),
        };
        writeln!(
            f,
            "{}- {} {}",
            vec![" "; depth * 2].join(""),
            self.name,
            description
        )?;
        if let PathKind::Dir(ls) = &self.kind {
            ls.iter()
                .map(|file| file.borrow().recurse(f, depth + 1))
                .collect::<Result<_, _>>()?;
        };
        Ok(())
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.recurse(f, 0)
    }
}

fn parse(input: &str) -> Option<Rc<RefCell<Path>>> {
    let mut lines = input.lines().peekable();
    let mut stack = vec![];
    while let Some(line) = lines.next() {
        match line.get(2..4)? {
            "cd" => {
                let dir = line.get(5..)?;
                match dir {
                    "/" => {
                        let root = Rc::new(RefCell::new(Path {
                            name: dir.to_string(),
                            kind: PathKind::Dir(vec![]),
                        }));
                        stack = vec![root.clone()];
                    }
                    ".." => {
                        stack.pop();
                    }
                    _ => {
                        if let PathKind::Dir(dirs) = &stack.last()?.clone().borrow().kind {
                            let path = dirs.iter().find(|n| n.borrow().name == dir)?;
                            stack.push(path.clone());
                        };
                    }
                };
            }
            "ls" => {
                let PathKind::Dir(ls) = &mut stack.last()?.borrow_mut().kind else {
                    continue;
                };
                while lines
                    .peek()
                    .map(|line| line.chars().next() != Some('$'))
                    .unwrap_or(false)
                {
                    let node = match lines.next().and_then(|line| line.trim().split_once(" "))? {
                        ("dir", name) => Rc::new(RefCell::new(Path {
                            name: name.to_string(),
                            kind: PathKind::Dir(vec![]),
                        })),
                        (size, name) => Rc::new(RefCell::new(Path {
                            name: name.to_string(),
                            kind: PathKind::File(size.parse().unwrap_or_default()),
                        })),
                    };
                    ls.push(node);
                }
            }
            _ => (),
        }
    }
    stack.first().cloned()
}

pub fn part_one(input: &str) -> usize {
    let Some(tree) = parse(input) else {
        return 0;
    };
    println!("{}", tree.borrow());
    let mut total = 0;
    let mut sum_smallest = |size: usize| {
        if size <= 100000 {
            total += size;
        }
    };
    size_search(&tree, &mut sum_smallest);
    total
}

pub fn part_two(input: &str) -> usize {
    let Some(tree) = parse(input) else {
        return 0;
    };
    let target = 30000000 + size_search(&tree, &mut |_| ()) - 70000000;
    let mut best = usize::MAX;
    let mut find_smallest = |size: usize| {
        if size >= target {
            best = best.min(size);
        }
    };
    size_search(&tree, &mut find_smallest);
    best
}

fn size_search(node: &Rc<RefCell<Path>>, f: &mut impl FnMut(usize)) -> usize {
    match &node.borrow().kind {
        PathKind::File(size) => *size,
        PathKind::Dir(ls) => {
            let size: usize = ls.iter().map(|file| size_search(file, f)).sum();
            f(size);
            size
        }
    }
}
