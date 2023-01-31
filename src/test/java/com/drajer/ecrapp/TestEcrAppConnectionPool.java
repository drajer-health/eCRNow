package com.drajer.ecrapp;

import com.zaxxer.hikari.HikariDataSource;
import org.junit.jupiter.api.Assertions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.web.client.RestTemplate;

import java.sql.Connection;

import static com.zaxxer.hikari.util.UtilityElf.quietlySleep;

@SpringBootTest(classes = EcrApp.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestPropertySource(locations = "classpath:application-test.properties")
public class TestEcrAppConnectionPool {

  @Autowired private RestTemplate restTemplate;

  @Autowired private HikariDataSource ds;

  @org.junit.jupiter.api.Test
  public void connectionTest() throws InterruptedException {
    Assertions.assertNotNull(restTemplate);
    System.out.println("ds : " + ds);

    try {
      Thread[] threads = new Thread[10];
      for (int i = 0; i < threads.length; i++) {
        threads[i] =
            new Thread(
                    () -> {
                      try {
                        Connection connection = ds.getConnection();
                        quietlySleep(1000);
                       connection.close();
                      } catch (Exception e) {
                        e.printStackTrace();
                      }
                    });
      }

      for (int i = 0; i < threads.length; i++) {
        threads[i].start();
      }

      for (int i = 0; i < threads.length; i++) {
        threads[i].join();
      }
    } catch(Exception e) {
      e.printStackTrace();
    }
  }
}
