Config {
  font = "xft:Ubuntu Mono-12",
  bgColor = "black",
  fgColor = "#e6744c" --"#cc8400",
  position = TopW L 100,

  commands = [
    Run Cpu [
      "-H","50",
      "--high","red"
    ] 10,

    Run Memory [
      "-t","Mem: <usedratio>%"
    ] 10,

    Run Date "%a %b %_d %l:%M" "date" 10,

    Run Battery [
      "-t", "<acstatus>: <left>%", 
      "--", 
      "-O", "AC",
      "-o", "Bat",
      "-h", "green",
      "-l", "red"
    ] 10,

    Run StdinReader
   ],

   sepChar = "%",
   alignSep = "}{",
   template = "%StdinReader% }{ %battery% | %cpu% | %memory% | %date% "
   -- | Vol: %myvolume%    <fc=#e6744c>%date%</fc>     "
}