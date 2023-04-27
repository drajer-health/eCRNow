package com.drajer.bsa.ehr.subscriptions;

import com.drajer.bsa.model.KarProcessingData;

/**
 *
 *
 * <h1>SubscriptionGeneratorService</h1>
 *
 * This class defines the interface to create and delete subscriptions in an EHR.
 *
 * @author nbashyam
 */
public interface SubscriptionGeneratorService {

  /**
   * The method is used to create subscriptions in the EHR.
   *
   * <p>The KarProcessingData will contain the following to enable subscription creation.
   * KnowledgeArtifact - The artifact for which subscriptions should be enabled.
   *
   * <p>HealthcareSetting - The setting for which the subscriptions have to be created. This will be
   * used to interact with the EHR by getting EHR Authorization information.
   *
   * <p>KnowledgeArtifactStatus - This contains the following information:
   *
   * <p>isActive which means that the Knowledge Artifact is active for the healthcare setting.
   * subscriptionsEnabled which means that subscriptions have to be created if the KAR is Active.
   * subscriptions which indicate the list of subscriptions already created.
   *
   * <p>When the createSubscriptions method is called, it will check to see if the KAR isActive and
   * if subscripitosn are enabled and if the subscriptions set is empty. If all these conditions are
   * met then new subscription topics are created and the set of subscriptions populated with the Id
   * of the SubscriptionTopic(s) created.
   *
   * <p>If the subscriptions are already populated then the method does nothing.
   *
   * @param kd The processing context which contains information such as KnowledgeArtifact and
   *     KnowledgeArtifactStatus.
   */
  public void createSubscriptions(KarProcessingData kd);

  /**
   * The method is used to delete subscriptions in the EHR.
   *
   * <p>The KarProcessingData will contain the following to enable subscription deletion.
   * KnowledgeArtifact - The artifact for which subscriptions should be deleted.
   *
   * <p>HealthcareSetting - The setting for which the subscriptions have to be deleted. This will be
   * used to interact with the EHR by getting EHR Authorization information.
   *
   * <p>KnowledgeArtifactStatus - This contains the following information:
   *
   * <p>isActive which means that the Knowledge Artifact is active for the healthcare setting.
   * subscriptionsEnabled which means that subscriptions have to be created if the KAR is Active.
   * subscriptions which indicate the list of subscriptions already created.
   *
   * <p>When the deleteSubscriptions method is called, it will delete the subscriptions identified
   * by the Ids in the subscriptions set. If the set is empty, there are no subscriptions to be
   * deleted.
   *
   * @param kd The processing context which contains information such as KnowledgeArtifact and
   *     KnowledgeArtifactStatus.
   */
  public void deleteSubscriptions(KarProcessingData kd);
}
