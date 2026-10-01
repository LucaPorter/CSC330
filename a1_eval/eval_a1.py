import sys
import tempfile, shutil
from pathlib import Path

from evaluator import *


heads = {
    1: """
    exception NotImplemented
    let is_older: (int * int * int) * (int * int * int) -> bool = fun _ -> raise NotImplemented
    let days_in_month : (int * int) -> int option = fun _ ->  raise NotImplemented
    let dates_in_month : (int * int) -> (int * int * int) list option = fun _ ->  raise NotImplemented
    let num_of_days : (int * int * int) -> int option = fun _ ->  raise NotImplemented
    let nth_day : (int * int) -> (int * int * int) option = fun _ ->  raise NotImplemented
    """,
    2: """
    exception NotImplemented
    let get_records : string -> 'a list = fun _ -> raise NotImplemented
    let avail : 'a -> int = fun _ -> raise NotImplemented
    let last : 'a -> (int * float) option = fun _ -> raise NotImplemented
    let minmax : 'a -> (int * float) option * (int * float) option = fun _ -> raise NotImplemented
    let summarize : 'a list * string -> string = fun _ -> raise NotImplemented
    let concat : string * string list -> string = fun _ -> raise NotImplemented
    """
}
testcases = {
    1: [("is_older 1990 1 1 1995 15 1", "true"),
        ("is_older 1996 2 21 1996 2 20", "false"),
        ("is_older 2020 1 1 2020 1 2", "true"),
        ("is_older 2020 1 1 2020 1 1", "false"),
        ("is_older 2020 1 2 2020 1 1", "false"),
        ("is_older 1900 1 21 1900 2 21", "true"),
        ("is_older 1900 2 21 1900 1 21", "false"),
        ("is_older 0 12 12 2000 12 12", "true"),
        ("days_in_month 1900 1", "31"),
        ("days_in_month 1904 2", "29"),
        ("days_in_month 2000 2", "29"),
        ("days_in_month 1900 2", "28"),
        ("days_in_month 1901 2", "28"),
        ("days_in_month 1900 9", "30"),
        ("days_in_month 1900 12", "31"),
        ("days_in_month 1900 14", "None"),
        ("days_in_month -16 14", "None"),
        ("dates_in_month -1 1", "None"),
        ("dates_in_month 1 13", "None"),
        ("dates_in_month 1 11", ','.join(f"1 11 {i+1}" for i in range(30))),
        ("dates_in_month 2000 2", ','.join(f"2000 2 {i+1}" for i in range(29))),
        ("dates_in_month 2001 2", ','.join(f"2001 2 {i+1}" for i in range(28))),
        ("dates_in_month 2005 3", ','.join(f"2005 3 {i+1}" for i in range(31))),
        ("num_of_days 1 1 1", "1"),
        ("num_of_days 2003 5 6", "126"),
        ("num_of_days 2003 5 31", "151"),
        ("num_of_days 2003 5 32", "None"),
        ("num_of_days 2003 15 31", "None"),
        ("num_of_days 2000 12 31", "366"),
        ("num_of_days 2001 12 31", "365"),
        ("nth_day 1 1", "1 1 1"),
        ("nth_day 2003 126", "2003 5 6"),
        ("nth_day 2003 151", "2003 5 31"),
        ("nth_day 2003 1000", "None"),
        ("nth_day 2000 366", "2000 12 31"),
        ("nth_day 2000 365", "2000 12 30"),
        ("nth_day 2001 366", "None"),
        ("nth_day 2001 365", "2001 12 31")],
    2: [("get_records test_small.csv", "ALB,Albania,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,226.0,85.0,22.6,7.8,12.7,33.2,20.6,0.4,0.1,3.1,7.8,0.5,2.3,2.4,2.4,2.9,3.3,2.3,3.6,3.4,2.0,1.9,1.6,3.5,-0.4,2.1,2.0,1.4,1.6,2.0;AND,Andorra,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_;FJI,Fiji,_,_,_,_,_,_,_,_,_,_,4.1,9.1,22.0,11.1,14.5,13.1,11.4,7.0,6.1,7.8,14.5,11.2,7.0,6.7,5.3,4.4,1.8,5.7,11.8,6.2,8.2,6.5,4.9,5.2,0.8,2.2,3.1,3.4,5.7,2.0,1.1,4.3,0.8,4.2,2.8,2.4,2.5,4.8,7.7,3.1,3.7,7.3,3.4,2.9,0.5,1.4,3.9,3.3,4.1,1.8,-2.6,0.2"),
        ("avail test.csv BIH", "16"),
        ("avail test.csv CAN", "62"),
        ("avail test.csv AND", "0"),
        ("avail test.csv CHN", "35"),
        ("last test.csv BIH", "2021 2.0"),
        ("last test.csv BRB", "2019 4.1"),
        ("last test.csv CAN", "2021 3.4"),
        ("last test.csv AND", "None"),
        ("last test.csv CHN", "2021 1.0"),
        ("minmax test.csv BIH", "2016 -1.6 2008 7.4"),
        ("minmax test.csv BRB", "1998 -1.3 1974 38.9"),
        ("minmax test.csv CAN", "1994 0.2 1981 12.5"),
        ("minmax test.csv AND", "None None"),
        ("summarize test.csv AND", "Country: Andorra (AND)\nRecords available: 0 years"),
        ("summarize test.csv BIH", "Country: Bosnia and Herzegovina (BIH)\nRecords available: 16 years\nLast record: 2021 with rate of 1.981639006%\nLowest rate: 2016 with rate of -1.5841%\nHighest rate: 2008 with rate of 7.427043103%"),
        ("summarize test.csv CAN", "Country: Canada (CAN)\nRecords available: 62 years\nLast record: 2021 with rate of 3.395193185%\nLowest rate: 1994 with rate of 0.165562914%\nHighest rate: 1981 with rate of 12.47161241%"),
        ("summarize test.csv BRB", "Country: Barbados (BRB)\nRecords available: 53 years\nLast record: 2019 with rate of 4.100289645%\nLowest rate: 1998 with rate of -1.268886607%\nHighest rate: 1974 with rate of 38.92292809%"),
        ("concat , a _ b c", "a,b,c"),
        ("concat , _ _ a _", "a"),
        ("concat , _ _ _", ""),
        ("concat ; a b c_d", "a;b;c_d"),
        ("concat ; x", "x")]
}
style_allowed_fns = {
    1: "Option.is_some Option.is_none Option.get List.hd List.tl".split(),
    2: "String.split_on_char Float.of_string_opt String.trim".split()
}
points = {
    1: { 'is_older': 5, 'days_in_month': 5, 'dates_in_month': 5,
         'num_of_days': 5, 'nth_day': 5 },
    2: { 'get_records': 5, 'avail': 5, 'last': 5,
         'minmax': 5, 'summarize': 5, 'concat': 5 },
}


def run(zip, id):
    print(f"## Evaluating Problem {id} in 🗂️ {zip}\n")

    if not (code := get_file(zip, id, "ml")):
        print(f"- ⛔️ Cannot find prob{id}.ml in the provided ZIP file!\n")
        return
    with tempfile.TemporaryDirectory() as tmp:
        if id == 2:
            shutil.copy("a1/test.csv", tmp + "/")
            shutil.copy("a1/test_small.csv", tmp + "/")
        t = ocaml_run(
            code, Path(tmp), testcases[id], id,
            f"a1/test_prob{id}.ml", heads[id], points[id],
            style_allowed_fns[id],
            ["match"] if id == 2 else []
        )
        summarize(*t)
        print()


if __name__ == "__main__":
    for prob in [1, 2]:
        run(sys.argv[1:], prob)
