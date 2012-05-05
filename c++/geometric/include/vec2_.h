namespace rovers
{


class Vec2
{
public:
	Vec2(int x, int y);
	int x() const;
	int y() const;

	bool operator==(const Vec2&) const;
	void operator+=(const Vec2&);
	void rotateRight();  // rotate 90 degrees clockwise about origin
	void rotateLeft();   // rotate 90 degrees anti-clockwise about origin

private:
	void setXY(int x, int y);

	int x_, y_;
};


}  // namespace rovers