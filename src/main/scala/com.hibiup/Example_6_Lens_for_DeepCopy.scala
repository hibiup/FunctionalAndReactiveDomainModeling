package com.hibiup

object Example_6_Lens_for_DeepCopy extends App{
    /** P-118(93) */

    /**************************************
     * 准备数据
     * */
    final case class Address(no: String, street: String, city: String, state: String, zip: String)
    final case class Customer(id: Int, name: String, address:Address)

    /** 获得实例 */
    val a = Address(no = "B-12", street = "Monroe Street", city = "Denver", state = "CO", zip = "80231")
    val c = Customer(12, "John D Cook", a)


    /**************************************
     * 定义 Lens 以实现对成员的访问
     * */
    final case class Lens[O, V]( get: O => V,
                           set: (O, V) => O )

    /** 用 Lens 来实现访问 Address 和 Customer 的成员的方法。第一个参数是要访问的对象类型，第二个是成员的值类型。*/
    val addressNumberLens = Lens[Address, String](
        /** 定义 getter, setter */
        get = _.no,
        set = (o, v) => o.copy(no = v)
    )
    val customerAddressLens = Lens[Customer, Address](
        get = _.address,
        set = (o, v) => o.copy(address = v)
    )

    /** 访问 Address 的成员 */
    assert("B-12" == addressNumberLens.get(a))

    val a1 = addressNumberLens.set(a, "B-56")
    assert("B-56" == addressNumberLens.get(a1))


    /**************************************
     *  定义组合函数, 支持“深拷贝”。　参数是两个 Lens 对象和要修改的成员的值类型。返回也是 Lens
     * */
    def compose[Outer, Inner, Value](outer: Lens[Outer, Inner],
                                     inner: Lens[Inner, Value] ) = Lens[Outer, Value](
        get = outer.get andThen inner.get,                                      // 用 andThen 连接两个运算： inner.get(outer.get(x))
        set = (obj, value) => outer.set(obj, inner.set(outer.get(obj), value))  // 返回新的 outer: Customer
    )

    /** 实现深拷贝 */
    val customerAddrNumberLens = compose(customerAddressLens, addressNumberLens)
    assert("B-12" == customerAddrNumberLens.get(c))

    val c1 = customerAddrNumberLens.set(c, "B675")
    assert("B675" == customerAddrNumberLens.get(c1))
}
