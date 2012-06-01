package ca.eandb.s99

/**
 * Created with IntelliJ IDEA.
 * User: brad
 * Date: 5/31/12
 * Time: 7:43 AM
 * To change this template use File | Settings | File Templates.
 */

case class S99Int(n: Int) {

  implicit def int2s99Int(n: Int) = S99Int(n)

}
