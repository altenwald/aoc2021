import advent06/posix_errors.{PosixError}

pub fn read_to_bitstring(
  filename filename: String,
) -> Result(BitString, PosixError) {
  do_read_to_bitstring(filename)
}

pub external fn do_read_to_bitstring(
  filename: String,
) -> Result(BitString, PosixError) =
  "file" "read_file"
