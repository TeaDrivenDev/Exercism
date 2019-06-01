pub fn raindrops(n: u32) -> String {
    let result = 
        (1..=n)
            .filter(|divisor| n % divisor == 0)
            .map(|divisor| sound(divisor))
            .collect::<Vec<&str>>()
            .concat();

    match result.as_ref() {
        "" => n.to_string(),
        _ => result
    }
}

fn sound (n: u32) -> &'static str {
    match n {
        3 => "Pling",
        5 => "Plang",
        7 => "Plong",
        _ => ""
    }
}