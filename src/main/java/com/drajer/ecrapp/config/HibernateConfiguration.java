package com.drajer.ecrapp.config;

import com.zaxxer.hikari.HikariDataSource;
import java.util.Properties;
import javax.sql.DataSource;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;
import org.springframework.orm.hibernate5.HibernateTransactionManager;
import org.springframework.orm.hibernate5.LocalSessionFactoryBean;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@EnableTransactionManagement
@PropertySource(value = {"classpath:application.properties"})
public class HibernateConfiguration {

  @Autowired private Environment environment;

  @Bean(name = "entityManagerFactory")
  public LocalSessionFactoryBean sessionFactory() {
    LocalSessionFactoryBean sessionFactory = new LocalSessionFactoryBean();
    sessionFactory.setDataSource(dataSource());
    sessionFactory.setPackagesToScan(
        "com.drajer.ersd.model",
        "com.drajer.sof.model",
        "com.drajer.eca.model",
        "com.drajer.ecrapp.model",
        "com.drajer.bsa.model",
        "com.drajer.bsa.kar.model");
    sessionFactory.setHibernateProperties(hibernateProperties());
    return sessionFactory;
  }

  @Bean
  public DataSource dataSource() {
    HikariDataSource dataSource = new HikariDataSource();
    dataSource.setUsername(environment.getRequiredProperty("jdbc.username"));
    dataSource.setPassword(environment.getRequiredProperty("jdbc.password"));
    dataSource.setJdbcUrl(environment.getRequiredProperty("jdbc.url"));
    dataSource.setDriverClassName(environment.getRequiredProperty("jdbc.driverClassName"));
    dataSource.setMaximumPoolSize(
        Integer.parseInt(environment.getRequiredProperty("hikari.maximum_pool_size")));
    dataSource.setMaxLifetime(
        Long.parseLong(environment.getRequiredProperty("hikari.max_lifetime")));
    dataSource.setIdleTimeout(
        Long.parseLong(environment.getRequiredProperty("hikari.idle_timeout")));
    dataSource.setConnectionTimeout(
        Long.parseLong(environment.getRequiredProperty("hikari.connection_timeout")));
    dataSource.setMinimumIdle(
        Integer.parseInt(environment.getRequiredProperty("hikari.minimum_idle")));
    dataSource.setValidationTimeout(
        Integer.parseInt(environment.getRequiredProperty("hikari.validation_timeout")));
    return dataSource;
  }

  private Properties hibernateProperties() {
    Properties properties = new Properties();
    // properties.put("hibernate.dialect", environment.getRequiredProperty("hibernate.dialect"));
    properties.put("hibernate.show_sql", environment.getRequiredProperty("hibernate.show_sql"));
    properties.put("hibernate.format_sql", environment.getRequiredProperty("hibernate.format_sql"));
    properties.put(
        "hibernate.hbm2ddl.auto", environment.getRequiredProperty("hibernate.hbm2ddl.auto"));
    properties.put(
        "hibernate.id.new_generator_mappings",
        environment.getRequiredProperty("hibernate.id.new_generator_mappings"));
    return properties;
  }

  @Bean
  @Autowired
  public HibernateTransactionManager transactionManager(SessionFactory s) {
    HibernateTransactionManager txManager = new HibernateTransactionManager();
    txManager.setSessionFactory(s);
    return txManager;
  }
}
