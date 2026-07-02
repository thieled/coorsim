#' Print TBB static library name for Makevars.win
if (getRversion() >= "4.3.0") {
    cat("-ltbb12")
} else {
    cat("-ltbb_static")
}
