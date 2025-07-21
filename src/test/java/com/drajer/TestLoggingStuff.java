package com.drajer;

import com.drajer.sof.launch.LaunchController;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestLoggingStuff {


    @Test
    public void testLoggingStuff() {
        Logger logger = LoggerFactory.getLogger("test-logger");
        logger.info("Test logging stuff");
        logger.info("Test logging stuff {}", "Bla%0ABla");
        logger.info("Test logging stuff {}", "Bla\r\nBla");
    }
}
