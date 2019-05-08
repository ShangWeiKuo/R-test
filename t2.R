# ---------
#  21-4
# ---------
# ---------------------------------------------------------- #

library(plyr)
baseball <- baseball[baseball$year >= 1990, ]
head(baseball)

# ---------------------------------------------------------- #

## �إߨ�ƥH�p�⥴���v
# data������
# boot�N�ǻ����P�ժ�����(indices)
# �b��@���ǻ���,���Ǿ�ƪ����޷|�X�{�X��
# ���ǫh�������|�X�{
# �����ӻ�63%����Ʒ|�X�{
# boot�N���Ʃʦa�I�s�����
bat.avg <- function(data, indices=1:NROW(data), hits="h",
                    at.bats="ab")
      {
         sum(data[indices, hits], na.rm=TRUE) /
         sum(data[indices, at.bats], na.rm=TRUE)
      }

# �έ��ƨӴ��ոӨ��
bat.avg(baseball)

# �}�l�ۧU���
# �ҥΪ���Ƭ�baseball���,��N�I�sbat.avg 1,200 ��
# �C���|����޶ǻ�����
avgBoot <- boot(data=baseball, statistic=bat.avg, R=1200, stype="i")

# ��ܹ���ƪ����q(original),���p�Ȫ����t(bias)�M�зǻ~�t
avgBoot

# ��ܫH��϶�
boot.ci(avgBoot, conf=.95, type="norm")

# ---------------------------------------------------------- #

ggplot() +
   geom_histogram(aes(x=avgBoot$t), fill="grey", color="grey") +
   geom_vline(xintercept=avgBoot$t0 + c(-1, 1)*2*sqrt(var(avgBoot$t)),
              linetype=2)

# ---------------------------------------------------------- #