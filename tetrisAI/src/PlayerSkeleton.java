import java.util.ArrayList;
import java.util.Random;


class m
{
	static float dot(float[] a, float[] b, int n)
	{
		float r=0;
		for(int i=0;i<n;i++)
		{
			r += a[i] * b[i];
		}
		
		return r;
	}
	
	static float norm(float[] a)
	{
		float r = dot(a,a,a.length);
		return (float) Math.sqrt(r);
	}
	
	static float[] sub(float[] a, float[] b, int n)
	{
		float[] r= new float[n];
		for(int i=0;i<n;i++)
		{
			r[i] = a[i] - b[i];
		}
		
		return r;
	}
	
	static float[] add(float[] a, float[] b, int n)
	{
		float[] r= new float[n];
		for(int i=0;i<n;i++)
		{
			r[i] = a[i] + b[i];
		}
		
		return r;
	}
	
	static float[] matmulvec(float A[][], float x[], int n)
	{
		float[] r = new float[n];
		for(int i=0;i<n;i++)
		{
			r[i] = dot(A[i],x,n);
		}
		return r;
	}
	
	static float[] copy(float[] a)
	{
		float r[] = new float[a.length];
		for(int i=0;i<a.length;i++)
		{
			r[i]= a[i];
		}
		return r;
	}
	
	static void copy(float[] to, float[] from, int n)
	{
		for(int i=0;i<n;i++)
		{
			to[i]= from[i];
		}
	}
	
	static float[][] inverse(float[][] A)
	{
		int N = A.length;
		int M = A[0].length;
		if(N!=M)
		{
			System.out.println("Not invertible");
			return new float[N][N];
		}
		else
		{
			float result[][] = new float[N][N * 2];

			//build [ A | I ]
			for(int i=0;i<N;i++)
				for(int j=0;j<N;j++)
					result[i][j] = A[i][j];

			for(int i=0;i<N;i++)
				for(int j=N;j<N * 2;j++)
					if(i==(j-N))
						result[i][j] = 1;
					else
						result[i][j] = 0;

			for(int i = 0; i < N; i++)
			{
				for(int j = 0; j < N; j++)
				{
					if(i!=j)
					{
						float ratio = result[j][i]/result[i][i];
						for(int k = 0; k < 2*N; k++)
							result[j][k] -= ratio * result[i][k];
					}
				}
			}

			for(int i = 0; i < N; i++)
			{
				float a = result[i][i];
				for(int j = 0; j < 2*N; j++)
					result[i][j] /= a;
			}
			
			float ret[][] = new float[N][N];

			for(int i=0;i<N;i++)
				for(int j=N;j<2*N;j++)
					ret[i][j-N] = result[i][j];
			
			return ret;
		}
	}
	
	static float[] solve(float[][] A, float b[])
	{
		int N = A.length;
		int M = A[0].length;
		if(N!=M)
		{
			System.out.println("Not invertible");
			return new float[N];
		}
		else
		{
			float result[][] = new float[N][N + 1];

			//build [ A | I ]
			for(int i=0;i<N;i++)
				for(int j=0;j<N;j++)
					result[i][j] = A[i][j];

			for(int i=0;i<N;i++)
				result[i][N] = b[i];

			for(int i = 0; i < N; i++)
			{
				for(int j = 0; j < N; j++)
				{
					if(i!=j)
					{
						float ratio = result[j][i]/result[i][i];
						for(int k = 0; k < N+1; k++)
							result[j][k] -= ratio * result[i][k];
					}
				}
			}

			for(int i = 0; i < N; i++)
			{
				float a = result[i][i];
				for(int j = 0; j < N+1; j++)
					result[i][j] /= a;
			}
			
			float ret[] = new float[N];

			for(int i=0;i<N;i++)
					ret[i] = result[i][N];
			
			return ret;
		}
	
	}
	
	static void GaussSeidel(float[][] A, float b[],float sol[])
	{
		float eps = 0.01f;
		int max_it = 100;
		int it = 0;
		float[] loco= new float[A.length];
		float oldnorm = 0;
		float newnorm = 0;
		do
		{
			for(int i=0;i<A.length;i++)
			{
				loco[i] = 0;
				for(int j=0;j<A[0].length;j++)
				{
					if(i!=j)
					{
						loco[i] += A[i][j] * sol[j];
					}
				}
				sol[i] = (1.0f/A[i][i]) * (b[i] - loco[i]);
			}
			oldnorm = newnorm;
			newnorm = m.norm(sol);
			it++;
		}while( Math.abs(newnorm-oldnorm) > (eps) && it < max_it);
		
		System.out.println("Done in "+it);

	}
	
	float[][] matMul(float[][] A, float [][] B)
	{
		int N = A.length, M = B[0].length;
		float[][] result = new float[N][M];
		if(A[0].length == B.length)
		{
			int K = B.length;
			
			for(int i=0;i<N;i++)
				for(int j=0;j<M;j++)
				{
					result[i][j] = 0;
					for(int k=0;k<K;k++)
					{
						result[i][j] += A[i][k] * B[k][j];
					}
				}
			return result;
		}
		else
		{
			System.out.println("Cannot multiply mat");
			return result;
		}
	}
	
	float[][] matMul(float[] A, float [] B)
	{
		int N = A.length, M = B.length;
		float[][] result = new float[N][M];
			
			for(int i=0;i<N;i++)
				for(int j=0;j<M;j++)
				{

						result[i][j] = A[i] * B[j];

				}
			return result;
			
	}
	
	float[][] matAdd(float[][] A, float[][] B)
	{
		float[][] r = new float[A.length][A[0].length];
		for(int i=0;i<A.length;i++)
			for(int j=0;j<A[0].length;j++)
				r[i][j] = A[i][j] + B[i][j];
		return r;
	}
	
	float[][] matSub(float[][] A, float[][] B)
	{
		float[][] r = new float[A.length][A[0].length];
		for(int i=0;i<A.length;i++)
			for(int j=0;j<A[0].length;j++)
				r[i][j] = A[i][j] - B[i][j];
		return r;
	}

}

class policy
{
	float w[];
	float current_reward;
	public policy(float weights[])
	{
		w = weights;
	}
	
	public int eval(State s)
	{
		int[][] moves = s.legalMoves();
		Random r = new Random(System.currentTimeMillis());
		int next_move = r.nextInt(moves.length);
		float max_reward=Float.MIN_VALUE;
		for(int i=0;i<moves.length;i++)
		{
			float[] next_basis = s.getBasisForMove(moves[i]);

			float expVal = 0;
			for(int j=0;j< next_basis.length;j++)
			{
				expVal += next_basis[j] * w[j];
			}
			
			if(expVal > max_reward)
			{
				max_reward = expVal;
				next_move = i;
			}
		}
		current_reward = max_reward;
		return next_move;
	}
}

class sample
{
	public State s, next_s;
	public int move;
	public float reward;
	
	public sample(State _s, int _move, float _reward, State _next_state)
	{
		s = _s;
		next_s = _next_state;
		move = _move;
		reward = _reward;
	}
}

class recorder
{
	ArrayList<sample> list;
	public recorder()
	{
		list = new ArrayList<sample>();
	}
	
	
	void add(sample s)
	{
		list.add(s);
	}
	
	sample[] getSampleSet()
	{
		return list.toArray(new sample[list.size()]);
	}
}

public class PlayerSkeleton {
	static final int no_basis = 10;
	static sample[] samples;
	static policy the_policy;

public static void printArr(float[] n)
{
	for(int i=0;i<n.length;i++)
	{
		System.out.print(n[i]+" ");
	}
	System.out.println("");
}

public void printMat(float[][] n)
{
	for(int i=0;i<n.length;i++)
	{
		for(int j=0;j<n[0].length;j++)
			System.out.print(n[i][j]+" ");
		System.out.println("");
	}
	System.out.println("");
}
	
public static float[] LSQ(sample[] D, int k, float discount, policy newp)
{
	float[] w;
	float A[][] = new float[k][k];
	float b[] = new float[k];
	
	for(int i=0;i<k;i++)
		for(int j=0;j<k;j++)
		{
			if(i==j)
				A[i][j]=0.0001f;
			else
				A[i][j]=0;
		}
	
	for(int i=0;i<k;i++)
		b[i]=0;
	
	for(int i=0;i<D.length;i++)
	{
		float[] next_basis = D[i].next_s.getBasisForMove(newp.eval(D[i].next_s));
		float[] basis = D[i].s.getBasisForMove(D[i].move);
		float q[] = new float[k];
		
		//printArr(basis);
		//printArr(next_basis);
		
		for(int x=0;x<k; x++)
		{
			q[x] = (basis[x] - discount * next_basis[x]);
		}
		
		//printArr(q);
		
		for(int x=0;x<k;x++)
			for(int y=0;y<k;y++)
			{
				A[x][y] += basis[x] * q[y]; 
			}
		
		for(int x=0;x<k;x++)
		{
			b[x] += basis[x] * D[i].reward;
		}
	}
	
	
	w = m.solve(A, b);

	//printMat(A);
	/*
	w = new float[k];
	
	for(int i=0;i<w.length;i++)
		w[i] = 0;
	
	m.GaussSeidel(A, b, w);
	*/
	//printArr(w);
	
	return w;
}

public static policy LSPI(float discount,float eps,policy init,sample[] D0)
{
	sample[] D = D0;
	policy p = init;
	policy old;
	
	printArr(p.w);
	
	do
	{
		//update D: how???
		old = new policy(p.w);
		p.w = LSQ(D,no_basis,discount,old);
	
	}while(Math.abs(m.norm(old.w) - m.norm(p.w)) > (eps * eps));
	
	return p;
}

	
	//implement this function to have a working system
	public int pickMove(State s, int[][] legalMoves)
	{
		//set current samples to current legal moves
		//reward r is rows cleared
		//discount factor is arbitrary?
		//probabilty of transition is 1/7
		
		///policy p = LSPI(0.98f,0.01f,old_policy,samples);
		///old_policy = p;
		//printArr(.w);
		
		return the_policy.eval(s);
	}
	
	//implement this function to have a working system
	public int HandCodePickMove(State s, int[][] legalMoves, float[] reward)
	{

		//set current samples to current legal moves
		//reward r is rows cleared
		//discount factor is arbitrary?
		//probabilty of transition is 1/7
		
		policy p = new policy(new float[]{0,2,0,-1,0,-6,0,0,0,-1});
		int move = p.eval(s);
		reward[0] = p.current_reward;
		
		return move;
	}
	
	static sample[] record(int lower_bound)
	{
		int rows_cleared;
		recorder rec;
		
		do
		{
		rec = new recorder();
		State olds, news;
		int move;
		int it=0;
		float discount = 0.5f;
		float reward[] = new float[1];
		
		State s = new State();
		PlayerSkeleton p = new PlayerSkeleton();
		while(!s.hasLost()) {
			it++;
			olds = s.clone();
			
			move = p.HandCodePickMove(s,s.legalMoves(),reward);
			
			s.makeMove(move);
			news = s.clone();
			
			rec.add(new sample(olds,move,reward[0],news));
		}
		rows_cleared = s.getRowsCleared();
		System.out.println("You have completed "+rows_cleared+" rows.");
		}while(rows_cleared < lower_bound);
		
		return rec.getSampleSet();
	}
	
	
	public static void main(String[] args) {
		
		policy old_policy = new policy(new float[]{0,0,0,0,0,0,0,0,0,0});
		
		 samples = record(500);
		the_policy = LSPI(0.87f,0.001f,old_policy,samples);
		printArr(the_policy.w);
		State s = new State();
		new TFrame(s);
		PlayerSkeleton p = new PlayerSkeleton();
		while(!s.hasLost()) {			
			
			s.makeMove(p.pickMove(s,s.legalMoves()));
		
			s.draw();
			s.drawNext(0,0);
			try {
				Thread.sleep(1);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			
		}
		System.out.println("You have completed "+s.getRowsCleared()+" rows.");
		
		/*
		float A[][] = { {5,4,-1},
						{0,10,-3},
						{0,0,1}};
		float b[] = {0,11,3};
		float sol[] = {0,0,0};
		m.GaussSeidel(A, b, sol);
		
		System.out.println(sol[0] + " " + sol[1] +  " " + sol[2]);	
		*/
		
	}
	
}
