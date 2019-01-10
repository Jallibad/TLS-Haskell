module RecordLayer.Alert where

import Data.Bimap (fromList)
import Data.Word (Word8)
import Utility.BinaryEnum

data AlertLevel = Warning | Fatal deriving (Eq, Ord, Show)
instance BinaryEnum AlertLevel Word8 where
	equivalences = fromList [(Warning, 1), (Fatal, 2)]

data AlertDescription	= CloseNotify
						| UnexpectedMessage
						| BadRecordMac
						| DecryptionFailedRESERVED
						| RecordOverflow
						| DecompressionFailure
						| HandshakeFailure
						| NoCertificateRESERVED
						| BadCertificate
						| UnsupportedCertificate
						| CertificateRevoked
						| CertificateExpired
						| CertificateUnknown
						| IllegalParameter
						| UnknownCa
						| AccessDenied
						| DecodeError
						| DecryptError
						| ExportRestrictionRESERVED
						| ProtocolVersion
						| InsufficientSecurity
						| InternalError
						| UserCancelled
						| NoRenegotiation
						| UnsupportedExtension
						deriving (Show, Ord, Eq, Enum)
instance BinaryEnum AlertDescription Word8 where
	equivalences = createEquivalences
		[0, 10, 20, 21, 22, 30, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 60, 70, 71, 80, 90, 100, 110]