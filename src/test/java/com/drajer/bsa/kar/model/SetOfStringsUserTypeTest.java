// package com.drajer.bsa.kar.model;
//
// import static org.junit.Assert.*;
// import static org.mockito.Mockito.*;
//
// import java.io.Serializable;
// import java.sql.PreparedStatement;
// import java.sql.ResultSet;
// import java.sql.Types;
// import java.util.HashSet;
// import java.util.Set;
// import org.hibernate.engine.spi.SharedSessionContractImplementor;
// import org.junit.Test;
//
// public class SetOfStringsUserTypeTest {
//
//  private final SetOfStringsUserType type = new SetOfStringsUserType();
//
//  @Test
//  public void testSqlTypes() {
//    int[] types = type.sqlTypes();
//    assertNotNull(types);
//    assertEquals(1, types.length);
//    assertEquals(Types.JAVA_OBJECT, types[0]);
//  }
//
//  @Test
//  public void testReturnedClass() {
//    Class<?> clazz = type.returnedClass();
//    assertEquals(Set.class, clazz);
//  }
//
//  @Test
//  public void testEqualsBothNull() {
//    assertTrue(type.equals(null, null));
//  }
//
//  @Test
//  public void testEqualsDifferentValues() {
//    assertFalse(type.equals("a", "b"));
//  }
//
//  @Test
//  public void testEqualsSameValues() {
//    assertTrue(type.equals("x", "x"));
//  }
//
//  @Test
//  public void testHashCode() {
//    String value = "test";
//    assertEquals(value.hashCode(), type.hashCode(value));
//  }
//
//  @Test
//  public void testNullSafeGetWithNullValue() throws Exception {
//    ResultSet rs = mock(ResultSet.class);
//    when(rs.getString("col")).thenReturn(null);
//
//    Object result =
//        type.nullSafeGet(
//            rs, new String[] {"col"}, mock(SharedSessionContractImplementor.class), null);
//
//    assertNull(result);
//  }
//
//  @Test
//  public void testNullSafeSetWithNonSetValue() throws Exception {
//    PreparedStatement ps = mock(PreparedStatement.class);
//
//    type.nullSafeSet(ps, "invalid", 1, mock(SharedSessionContractImplementor.class));
//
//    verify(ps).setNull(1, Types.OTHER);
//  }
//
//  @Test
//  public void testNullSafeSetWithSetValue() throws Exception {
//    PreparedStatement ps = mock(PreparedStatement.class);
//    Set<String> input = new HashSet<>();
//    input.add("x");
//    input.add("y");
//
//    type.nullSafeSet(ps, input, 1, mock(SharedSessionContractImplementor.class));
//
//    verify(ps).setObject(eq(1), eq("x||y"), eq(Types.OTHER));
//  }
//
//  @Test
//  public void testIsMutable() {
//    assertTrue(type.isMutable());
//  }
//
//  @Test
//  public void testDeepCopy() {
//    Object obj = new Object();
//    Object copy = type.deepCopy(obj);
//    assertSame(obj, copy);
//  }
//
//  @Test
//  public void testDisassemble() {
//    Serializable value = "value";
//    Serializable result = type.disassemble(value);
//    assertEquals(value, result);
//  }
//
//  @Test
//  public void testAssemble() {
//    Serializable cached = "cached";
//    Object result = type.assemble(cached, null);
//    assertEquals(cached, result);
//  }
//
//  @Test
//  public void testReplace() {
//    Object original = "original";
//    Object target = "target";
//
//    Object result = type.replace(original, target, null);
//
//    assertEquals(original, result);
//  }
//
//  @Test
//  public void testNullSafeGet_splitBehaviorCoverage() throws Exception {
//    ResultSet rs = mock(ResultSet.class);
//    when(rs.getString("col")).thenReturn("ab");
//
//    Object result =
//        type.nullSafeGet(
//            rs, new String[] {"col"}, mock(SharedSessionContractImplementor.class), null);
//
//    assertNotNull(result);
//    assertTrue(result instanceof Set);
//
//    Set<?> set = (Set<?>) result;
//
//    // split("||") splits every character
//    assertTrue(set.contains("a"));
//    assertTrue(set.contains("b"));
//  }
// }
