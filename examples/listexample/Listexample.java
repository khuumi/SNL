public class Listexample{

	private static SNLObject lst;
	private static SNLObject changedlst;
    private static SNLObject i;

	public static void main(String args[]){
		example_program();
	}

	private static void example_program(){

        SNLObject tmp = new SNLObject("list", new SNLObject(3, "int"), new SNLObject(4.2, "float"), new SNLObject("hello", "string"), new SNLObject(1<2, "bool"));
        tmp.app(new SNLObject(false, "bool"));
        System.out.println(tmp);
        lst = new SNLObject(tmp);

		changedlst = Recipe_rotate_list.perform(lst);

		System.out.println(lst);
		System.out.println(changedlst);   
        i = changedlst.getArr()[new SNLObject(1, "int").getInt()];
        changedlst.getArr()[new SNLObject(0, "int").getInt()] = new SNLObject("first!", "string");
        System.out.println(changedlst);
	}
}
