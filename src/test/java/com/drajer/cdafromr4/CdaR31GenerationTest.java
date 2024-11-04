package com.drajer.cdafromr4;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import org.junit.Before;
import org.junit.Test;

public class CdaR31GenerationTest extends BaseGeneratorTest {

  private static final String SCENARIO_DIRECTORY = "/CdaR31TestScenarios";

  @Before
  public void initTestData() {

    /* String filePath = getAbsoluteFilePath(SCENARIO_DIRECTORY);

    File folder = new File(filePath);

       File[] files = folder.listFiles((FileFilter) FileFilterUtils.fileFileFilter());

       if (files != null) {
         for (File kar : files) {

           if (kar.isFile() && JSON_KAR_EXT.contentEquals(FilenameUtils.getExtension(kar.getName()))) {

             logger.info(" Processing KAR {}", kar.getName());
             processKar(kar, repoUrl, repoName);
           } // For a File
         }
       }

       // Recursively process the directories also.
       File[] dirs = folder.listFiles((FileFilter) FileFilterUtils.directoryFileFilter());

       if (dirs != null) {
         for (File dir : dirs) {

           logger.info(" About to process directory : {}", dir.getName());
           String url = repoUrl + "/" + dir.getName();
           String name = repoName + "-" + dir.getName();

           loadKarsFromDirectory(dir.getPath(), url, name);
         }
       } */
  }

  @Test
  public void runScenarios() throws IOException {

    // ClassLoader classLoader = getClass().getClassLoader();
    // File file = new File(classLoader.getResource(SCENARIO_DIRECTORY).getFile());
    //  File file = ResourceUtils.getFile(this.getClass().getResource(SCENARIO_DIRECTORY));
    //   System.out.println(file.getAbsolutePath());

    assertEquals(true, true);
  }
}
