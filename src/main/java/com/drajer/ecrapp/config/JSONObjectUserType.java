package com.drajer.ecrapp.config;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import org.hibernate.engine.spi.SharedSessionContractImplementor;

public class JSONObjectUserType {

  /**
   * Return the SQL type codes for the columns mapped by this type. The codes are defined on
   * <tt>java.sql.Types</tt>.
   *
   * @return int[] the typecodes
   * @see java.sql.Types
   */
  public int[] sqlTypes() {
    return new int[] {Types.JAVA_OBJECT};
  }

  /**
   * The class returned by <tt>nullSafeGet()</tt>.
   *
   * @return Class
   */
  public Class<String> returnedClass() {
    return String.class;
  }

  /**
   * Compare two instances of the class mapped by this type for persistence "equality". Equality of
   * the persistent state.
   *
   * @param x
   * @param y
   * @return boolean
   */
  public boolean equals(Object x, Object y) {

    if (x == null) {

      return y == null;
    }
    return x.equals(y);
  }

  /** Get a hashcode for the instance, consistent with persistence "equality" */
  public int hashCode(Object x) {

    return x.hashCode();
  }

  /**
   * Retrieve an instance of the mapped class from a JDBC resultset. Implementors should handle
   * possibility of null values.
   *
   * @param rs a JDBC result set
   * @param names the column names
   * @param session
   * @param owner the containing entity @return Object
   * @throws org.hibernate.HibernateException
   * @throws java.sql.SQLException
   */
  public Object nullSafeGet(
      ResultSet rs, String[] names, SharedSessionContractImplementor session, Object owner)
      throws SQLException {
    if (rs.getString(names[0]) == null) {
      return null;
    }
    return rs.getString(names[0]);
  }

  /**
   * Write an instance of the mapped class to a prepared statement. Implementors should handle
   * possibility of null values. A multi-column type should be written to parameters starting from
   * <tt>index</tt>.
   *
   * @param st a JDBC prepared statement
   * @param value the object to write
   * @param index statement parameter index
   * @param session
   * @throws org.hibernate.HibernateException
   * @throws java.sql.SQLException
   */
  public void nullSafeSet(
      PreparedStatement st, Object value, int index, SharedSessionContractImplementor session)
      throws SQLException {
    if (value == null) {
      st.setNull(index, Types.OTHER);
      return;
    }
    st.setObject(index, value, Types.OTHER);
  }

  /**
   * Return a deep copy of the persistent state, stopping at entities and at collections. It is not
   * necessary to copy immutable objects, or null values, in which case it is safe to simply return
   * the argument.
   *
   * @param value the object to be cloned, which may be null
   * @return Object a copy
   */
  public Object deepCopy(Object value) {

    return value;
  }

  /**
   * Are objects of this type mutable?
   *
   * @return boolean
   */
  public boolean isMutable() {
    return true;
  }

  /**
   * Transform the object into its cacheable representation. At the very least this method should
   * perform a deep copy if the type is mutable. That may not be enough for some implementations,
   * however; for example, associations must be cached as identifier values. (optional operation)
   *
   * @param value the object to be cached
   * @return a cachable representation of the object
   * @throws org.hibernate.HibernateException
   */
  public Serializable disassemble(Object value) {
    return (String) this.deepCopy(value);
  }

  /**
   * Reconstruct an object from the cacheable representation. At the very least this method should
   * perform a deep copy if the type is mutable. (optional operation)
   *
   * @param cached the object to be cached
   * @param owner the owner of the cached object
   * @return a reconstructed object from the cachable representation
   * @throws org.hibernate.HibernateException
   */
  public Object assemble(Serializable cached, Object owner) {
    return this.deepCopy(cached);
  }

  /**
   * During merge, replace the existing (target) value in the entity we are merging to with a new
   * (original) value from the detached entity we are merging. For immutable objects, or null
   * values, it is safe to simply return the first parameter. For mutable objects, it is safe to
   * return a copy of the first parameter. For objects with component values, it might make sense to
   * recursively replace component values.
   *
   * @param original the value from the detached entity being merged
   * @param target the value in the managed entity
   * @return the value to be merged
   */
  public Object replace(Object original, Object target, Object owner) {
    return original;
  }
}
