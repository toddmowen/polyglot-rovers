enum Direction
{
	N ( 0,  1),
	E ( 1,  0),
	S ( 0, -1),
	W (-1,  0);

	public final int x, y;
	
	private Direction(int x, int y)
	{
		this.x = x;
		this.y = y;
	}
	
	public Direction right()
	{
		return Direction.values()[(this.ordinal() + 1) % 4];
	}
}

class Rover
{
	private int x;
	private int y;
	private Direction dir;
	
	public Rover(int x, int y, Direction dir)
	{
		this.x = x;
		this.y = y;
		this.dir = dir;
	}
	
	public String toString()
	{
		return String.format("%d %d %s", x, y, dir.toString());
	}
	
	public void execute(String commands)
	{
		for (char command : commands.toCharArray())
		{
			this.execute(command);
		}
	}
	
	public void execute(char command)
	{
		switch (Character.toUpperCase(command))
		{
		case 'R':
			dir = dir.right();
			break;
			
		case 'L':
			dir = dir.right().right().right();
			break;
			
		case 'M':
			x += dir.x;
			y += dir.y;
			break;
			
		default:
			throw new RuntimeException(String.format("Unknown command: %c", command));
		}
	}
}

public class Rovers
{
	
}