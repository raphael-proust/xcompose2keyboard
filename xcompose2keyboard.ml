
type seq =
	| S2 of char * char
	| S3 of char * char * char
type seqs = seq * seq list
type entry = {
	scalar: Uutf.uchar;
	sequences: seq * seq list;
	comment: string;
}

let string_of_sequences (seq, seqs) =
	String.concat " "
		(List.map (function
			| S2 (c1,c2) -> let s = String.make 2 c1 in s.[1] <- c2; s
			| S3 (c1,c2,c3) -> let s = String.make 3 c1 in s.[1] <- c2; s.[2] <- c3; s
			)
			(seq :: seqs)
		)

let string_of_rune uchar = failwith "TODO"

let print { scalar; sequences; comment; } =
	Printf.printf "%04X %s	%s	%s"
		scalar
		(string_of_sequences sequences)
		(string_of_rune scalar)
		comment

exception Sink

let translation = [
	"bar", "|";
	"plus", "+";
	"colon", ":";
	(*TODO*)
]

let entries =
	let decoder = Uutf.decoder (`Channel stdin) in
	let get c = match Uutf.decode decoder with
		| `End | `Malformed _ -> raise Sink
		| `Uchar u -> if !(u=c) then raise Sink
		| `Await -> failwith "TODO: better error mngmt"
	in
	let rec ignore_line () = match Uutf.decode decoder with
		| `End -> ()
		| `Uchar 0X000D | `Uchar 0X000A | `Uchar 0X0085 -> ()
		| `Malformed _ | `Uchar _ -> ignore_line ()
		| `Await -> failwith "TODO: better error mngmt"
	in
	let ignore_multi_key () =
		String.iter (fun c -> get (int_of_char c)) "Multi_key> "
	in
	let rec parse_sequence e = failwith "TODO" in
	let rec parse_rune e =  failwith "TODO" in
	let rec parse_comment e =  failwith "TODO" in
	let rec parse_line e =
		ignore_multi_key () ;
		let e = parse_sequence e in
		let e = parse_rune e in
		ignore_scalar () ;
		let e = parse_comment e in
		e
	in
	let rec loop accu = match Uutf.decode decoder with
		| `End -> accu
		| `Uchar c -> if c = int_of_char '<' then parse_line None else ignore_line ()
		| `Uchar _ | `Malformed _ -> ignore_line ()
	in
	loop []

let () = List.iter print entries
