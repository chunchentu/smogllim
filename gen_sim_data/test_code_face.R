library(R.matlab)
library(mclust)

temp = readMat("facedata_norm.mat")
yapp = temp$Y
tapp = temp$t.norm

temp = readMat("face_cluster7.mat")
cluster_member = temp$cluster.member

temp_t = tapp[, cluster_member]

# graph for checking
d = data.frame(t(temp_t))
colnames(d) = c("Light", "Pan", "Tilt")
pairs(~Light+Pan+Tilt, data=d)


# Mclust results
temp_cluster = Mclust(t(temp_t), 2, verbose=FALSE)
plot(temp_cluster, what="classification")

# index for two sub-clusters
subcluster_1 = cluster_member[which(temp_cluster$classification==1)]
subcluster_2 = cluster_member[which(temp_cluster$classification==2)]

writeMat("R_subcluster.mat", subcluster_1=subcluster_1, subcluster_2=subcluster_2)



