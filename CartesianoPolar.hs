import Text.Printf (printf)
mensagem x = printf "%.2f" x
angulo a b = ((atan (b/a))*180)/pi

parapolar a b = putStrLn ("P("++mensagem((sqrt((a**2)+(b**2))))++","++mensagem((angulo a b))++"°)")

paracartes c d = putStrLn("P("++mensagem (cos (d*pi/180)*c)++","++mensagem (sin (d*pi/180)*c)++")")