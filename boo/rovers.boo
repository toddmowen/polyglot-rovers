namespace rovers


enum Bearing:
	N
	E
	S
	W


class Rover:
	[Getter(X)]
	_x as int

	[Getter(Y)]
	_y as int

	[Getter(Bearing)]
	_bearing as Bearing

	def constructor(X as int, Y as int, Bearing as Bearing):
		_x = X
		_y = Y
		_bearing = Bearing

