public class Listexample{

	private static SNLObject lst;
	private static SNLObject changedlst;

	public static void main(String args[]){
		example_program();
	}

	private static void example_program(){

		lst = new SNLObject("list");
		lst.app(new SNLObject(3, "int"));
		lst.app(new SNLObject(4.2, "float"));
		lst.app(new SNLObject("hello", "string"));
		lst.app(new SNLObject(1<2, "bool"));

		changedlst = Recipe_rotate_list.perform(lst);

		System.out.println(lst);
		System.out.println(changedlst);		
	}
}
