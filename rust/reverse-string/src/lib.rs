use unicode_segmentation::UnicodeSegmentation;

pub fn reverse(input: &str) -> String {
    input.graphemes(true)
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect::<String>()
}
