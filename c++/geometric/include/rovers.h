namespace rovers
{


class Vec2
{
public:
	Vec2(int x, int y);
	int x() const;
	int y() const;

private:
	int x_, y_;
};


}  // namespace rovers