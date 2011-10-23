namespace rovers


enum Bearing:
	# Must start from zero, and be in clockwise order.
	N
	E
	S
	W


[Extension]
def Right(b as Bearing):
	return ((b cast int + 1) % 4) cast Bearing


[Extension]
def Left(b as Bearing):
	return ((b cast int + 3) % 4) cast Bearing


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

	def ToString():
		return "Rover($X, $Y, $Bearing)"

	def L():
		_bearing = _bearing.Right()

	def R():
		_bearing = _bearing.Left()
