use std::{collections::HashMap, fs};

fn get_size(pwd: &str, dirs: &HashMap<String, (Vec<String>, HashMap<String, i32>)>) -> i32 {
    let (subdirs, files) = dirs.get(pwd).unwrap();
    let files_size = files.values().sum();
    if subdirs.is_empty() {
        return files_size;
    }
    let subdirs_size = subdirs
        .into_iter()
        .map(|subdir| get_size(&subdir, dirs))
        .sum::<i32>();
    subdirs_size + files_size
}

fn dirs(input: &str) -> HashMap<String, (Vec<String>, HashMap<String, i32>)> {
    let mut dir_sizes: HashMap<String, (Vec<String>, HashMap<String, i32>)> = HashMap::new();
    dir_sizes.insert("/".to_string(), (Vec::new(), HashMap::new()));
    let mut path = Vec::new();
    for cmd in input.trim().lines() {
        if cmd == "$ cd .." {
            path.pop();
        } else if cmd == "$ ls" {
            continue;
        } else if cmd.starts_with("$ cd") {
            let dir = scan_fmt::scan_fmt!(cmd, "$ cd {}", String).unwrap();
            path.push(dir);
            let pwd = path.join("/");
            assert!(dir_sizes.contains_key(&pwd), "{:?} {:?}", pwd, path);
        } else if cmd.starts_with("dir") {
            let pwd = path.join("/");
            let dir = scan_fmt::scan_fmt!(cmd, "dir {}", String).unwrap();
            let dir = format!("{}/{}", pwd, dir);
            dir_sizes.insert(dir.clone(), (Vec::new(), HashMap::new()));
            let (subdirs, _) = dir_sizes.get_mut(&pwd).unwrap();
            subdirs.push(dir);
        } else {
            let pwd = path.join("/");
            let (size, name) = scan_fmt::scan_fmt!(cmd, "{d} {}", i32, String).unwrap();
            let (_, files) = dir_sizes.get_mut(&pwd).unwrap();
            files.insert(name, size);
        }
    }
    dir_sizes
}

fn part1(input: &str) -> i32 {
    let content = fs::read_to_string(input).expect("failed");
    let dir_sizes = dirs(&content);

    dir_sizes
        .keys()
        .map(|dir| get_size(dir, &dir_sizes))
        .filter(|sz| *sz < 100000)
        .sum()
}

fn part2(input: &str) -> i32 {
    let content = fs::read_to_string(input).expect("failed");
    let dir_sizes = dirs(&content);
    let unused = 70000000 - get_size("/", &dir_sizes);
    let min = 30000000 - unused;

    dir_sizes
        .keys()
        .map(|dir| get_size(dir, &dir_sizes))
        .filter(|sz| *sz > min)
        .min()
        .unwrap()
}

fn main() {
    let result1 = part1("input.txt");
    let result2 = part2("input.txt");

    println!("Result 1: {result1:?}");
    println!("Result 2: {result2}")
}
