namespace rovers
{


class Vec2
{
public:
	Vec2(int x, int y);
	int x() const;
	int y() const;

	bool operator==(const Vec2&) const;

private:
	int x_, y_;
};


class Rover
{
public:
	Rover(int x, int y, Vec2 const& velocity);
	int x() const;
	int y() const;
	Vec2 velocity() const;

private:
	Vec2 position_;
	Vec2 velocity_;
};


}  // namespace rovers