public class Listexample{

	public static void main(String args[]){
		example_program();
	}	

	private static void example_program(){
		// instance of Objects to allow for multiple types
		Object[] lst = new Object[4];
		// add all of the different elements with cast
		// to trick Java compiler but allow runtime environment
		// to still act correctly
		lst[0] = (Object) new Integer(3);
		lst[1] = (Object) new Double(4.2);
		lst[2] = (Object) new String("hello");
		lst[3] = (Object) new Boolean(true);

		Object[] changedlst = Recipe_rotate_list.perform(lst);
		
		// print will create this block of code and for loop if a list
		System.out.print("[");
		for(int i=0; i<lst.length-1; i++){
			System.out.print(lst[i] + ", ");
		}
		System.out.println(lst[lst.length-1] + "]");

		System.out.print("[");
		for(int i=0; i<changedlst.length-1; i++){
			System.out.print(changedlst[i] + ", ");
		}
		System.out.println(changedlst[changedlst.length-1] + "]");
		
	}


}
