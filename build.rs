use std::{env, fs};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

// use indexmap::IndexMap;

// #[path = "src/word/seg.rs"]
// mod segment;

fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-changed=src/cardinals.json");

    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("codegen.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());

    let cardinals_json = fs::read_to_string("src/cardinals.json")
        .expect("Failed to read src/cardinals.json");

    let parsed: serde_json::Value = serde_json::from_str(&cardinals_json)
        .expect("Failed to parse JSON");

    let mut map = phf_codegen::OrderedMap::new();

    if let Some(obj) = parsed.as_object() {
        for (key, value) in obj {
            let mut seg = String::from("Segment { ");
            if let Some(val_obj) = value.as_object() {
                for (k, v) in val_obj {
                    match k.as_str() {
                        "class" => seg.push_str(&format!("{k}: Class({v}), ")),
                        "place" => {
                            if v.as_null().is_some() {
                                seg.push_str(&format!("{k}: Place(std::num::NonZero::new(0)), "))
                            } else {
                                seg.push_str(&format!("{k}: Place(std::num::NonZero::new({v})), "))
                            }
                        },
                        _ => {}
                    }
                }
            }
            seg.push('}');

            map.entry(key.as_str(), seg);
        }
    }

    // println!("cargo::warning=Json successfully parsed");

    writeln!(
        &mut file,
        "pub static CARDINALS_MAP: phf::OrderedMap<&'static str, Segment> = {};",
        map.build()
    )
    .unwrap();
}