import numpy as np
from sklearn.metrics import silhouette_score

# GRADED CLASS: KMeans

class KMeans(object):
    """
    Parameters:
    -----------
    X -- np.array
        Matrix of input features
    k -- int
        Number of clusters
    """
    
    def __init__(self, X, k):
        self.X = X
        self.k = k
        
    def initialize_centroids(self):
        """ 
        Returns:
        
        Array of shape (k, n_features), 
            containing k centroids from the initial points
        """
        
        
        # use shuffle with random state = 512, and pick first k points
        X_copy = np.copy(self.X)
        np.random.RandomState(512).shuffle( X_copy)
        return X_copy[:self.k]
        
             
    def closest_centroid(self, centroids):
        """
        Returns:
        
        Array of shape (n_examples, ), 
            containing index of the nearest centroid for each point
        """
        
        
        return [np.argsort([np.linalg.norm(centr - item) for centr in centroids ])[0] for item in self.X]
        
    
    def move_centroids(self, centroids):
        """
        Returns:
        
        Array of shape (n_clusters, n_features),
        containing the new centroids assigned from the points closest to them
        """
        
        
        closest = np.array(self.closest_centroid(centroids))
        for index in range(len(centroids)):        
          values = np.take(self.X, np.where(closest==index),axis=0)
          quantity = np.where(closest==index)[0].size
          temp = values.T
          for coord in temp:
              coord_sum = np.sum(coord)
              centroids[index] =  coord_sum/quantity
        return np.copy(centroids)
        
        

    def final_centroids(self):
        """
        Returns:
        
        clusters -- list of arrays, containing points of each cluster
        centroids -- array of shape (n_clusters, n_features),
            containing final centroids 
        
        """
        
        
        centroids = self.initialize_centroids()
        centroids = self.move_centroids(centroids)
        while(True):
          centroids_bcp = np.copy(centroids)
          centroids = self.move_centroids(centroids)
          if np.array_equal(centroids_bcp,centroids):
            break
        
        closest = np.array(self.closest_centroid(centroids))
        clusters = [np.take(self.X,np.where(closest==index),axis=0).squeeze() for index in range(len(centroids))]
        

        return clusters, centroids

# GRADED FUNCTION: mean_distances

def mean_distances(k, X):
    """
    Arguments:
    
    k -- int, number of clusters
    X -- np.array, matrix of input features
    
    Returns:
    
    Array of shape (k, ), containing mean of sum distances 
        from centroid to each point in the cluster for k clusters
    """
    

    result = []
    for x in range(1,k+1):
      model = KMeans(X, x)
      clusters, final_centrs = model.final_centroids()
      result.append(sum([np.sum([np.linalg.norm(item-final_centrs[index]) for item in clusters[index]])/x for index in range(x)]))
      #print(x)
      #print([sum([np.linalg.norm(item-final_centrs[index]) for item in clusters[index]])/x for index in range(x)])
      

    return result
    


def determine_k(k, X):
    sil = []
    kmax = 10

    # dissimilarity would not be defined for a single cluster, thus, minimum number of clusters should be 2
    for x in range(2, k+1):
      kmeans = KMeans(X, x)
      clusters, final_centrs = kmeans.final_centroids()

      closest = kmeans.closest_centroid(final_centrs)
      sil.append(silhouette_score(X, closest, metric = 'euclidean'))
    return list([x for x in range(2, k)])[sil.index(np.max(sil))]

# For test

np.random.seed(37)
X = np.vstack(((np.random.randn(2, 58)  + np.array([0 for x in range (58)])),
               (np.random.randn(2, 58)  + np.array([0 for x in range (58)])),
               (np.random.randn(2, 58)  + np.array([0 for x in range (58)])),
               (np.random.randn(2, 58)  + np.array([0 for x in range (58)])),
               (np.random.randn(2, 58)  + np.array([0 for x in range (58)]))))

print('X.shape:', X.shape)
'''
print('Mean distances: ', mean_distances(10, X)) 

print('Mean distances (siloette): ', determine_k(10, X)) 
model = KMeans(X, 3)
centroids = model.initialize_centroids()
print('Random centroids:', centroids)

closest = model.closest_centroid(centroids)
print('Closest centroids:', closest[:10])

next_centroids = model.move_centroids(centroids)
print('Next centroids:', next_centroids)

clusters, final_centrs = model.final_centroids()
print('Final centroids:', final_centrs)
print('Clusters points:', clusters[0][0], clusters[1][0], clusters[2][0])
'''

def main_procedure(X, max_k = 10):

    print(X.shape)

    k = determine_k(max_k, X)
    print(f"Best match of k is :: {k}") 
    kmeans = KMeans(X, k)
    clusters, final_centrs = kmeans.final_centroids()
    closest = kmeans.closest_centroid(final_centrs)
    print(f"Result, {len(closest)}")
    #print(closest)
    return closest
    ...

main_procedure(X)